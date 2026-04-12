package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.request.PermissionRequestRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionAutoApproveService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class PermissionRequestServiceImpl implements PermissionRequestService {

    private final PermissionRequestMapper permissionRequestMapper;
    private final PermissionRequestItemMapper permissionRequestItemMapper;
    private final PermissionAutoApproveService permissionAutoApproveService;
    private final AuthorizeService authorizeService;
    private final PermissionRequestRepository permissionRequestRepository;

    /**
     * 提交权限申请
     *
     * @param dto
     *            申请请求
     * @return 申请结果
     */
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.PERMISSION_REQUEST, sysOperation = SysOperationType.CREATE, success = "提交了权限申请（{{ #result.requestId }}）", fail = "提交权限申请失败")
    @Transactional
    @Override
    public PermissionRequestResponseDto submitRequest(PermissionRequestCreateDto dto) {
        String userId = AuthUtil.getCurrentUserId();
        List<String> permissionIds = dto.getPermissionIds();

        // 1. 重复申请检查（per D-05）
        if (permissionRequestRepository.hasActivePendingRequest(userId, permissionIds)) {
            throw new IllegalStateException("您已有待审批或自动批准的相同权限申请，请勿重复提交");
        }

        String requestId = CommonUtil.getUUIDV7String();
        LocalDateTime now = LocalDateTime.now();

        // 2. 判断每个权限是否自动批准，构建申请明细列表
        List<PermissionRequestItem> items = new ArrayList<>();
        List<String> autoApprovedPermIds = new ArrayList<>();
        for (String permId : permissionIds) {
            boolean autoApproved = permissionAutoApproveService.isEnabled(permId);
            PermissionRequestItem item = new PermissionRequestItem();
            item.setItemId(CommonUtil.getUUIDV7String());
            item.setRequestId(requestId);
            item.setPermissionId(permId);
            item.setAutoApproved(autoApproved);
            items.add(item);
            if (autoApproved) {
                autoApprovedPermIds.add(permId);
            }
        }

        // 3. 确定申请主记录状态：全部自动批准则 AUTO_APPROVED，否则 PENDING
        boolean allAutoApproved = autoApprovedPermIds.size() == permissionIds.size();
        String status = allAutoApproved
                ? PermissionRequestStatusEnum.AUTO_APPROVED.getCode()
                : PermissionRequestStatusEnum.PENDING.getCode();

        // 4. 插入申请主记录（per D-03：在同一事务内）
        PermissionRequest request = new PermissionRequest();
        request.setRequestId(requestId);
        request.setUserId(userId);
        request.setReason(dto.getReason());
        request.setStatus(status);
        request.setRequestTime(now);
        permissionRequestMapper.insert(request);

        // 5. 插入申请明细记录（per D-04：批量全部成功或全部回滚）
        for (PermissionRequestItem item : items) {
            permissionRequestItemMapper.insert(item);
        }

        // 6. 自动批准权限写入 t_authorize（per PAUT-03，@CacheEvict 由 AuthorizeService 内部处理）
        if (!autoApprovedPermIds.isEmpty()) {
            AuthorizeRequestDto authDto = new AuthorizeRequestDto();
            authDto.setUserIds(List.of(userId));
            authDto.setPermissionIds(autoApprovedPermIds);
            // expressionIds 和 priority 保持 null，Phase 3 不设置限制条件
            authorizeService.authorize(authDto);
        }

        // 7. 构建并返回响应
        PermissionRequestResponseDto response = new PermissionRequestResponseDto();
        response.setRequestId(requestId);
        response.setAutoApprovedCount(autoApprovedPermIds.size());
        response.setPendingCount(permissionIds.size() - autoApprovedPermIds.size());
        return response;
    }

    @Override
    public PageData<PermissionRequestListItemDto> listUserRequests(int page, int size) {
        String userId = AuthUtil.getCurrentUserId();
        PageData<PermissionRequest> paged = permissionRequestRepository.findByUserIdPaged(userId, page, size);

        List<PermissionRequestListItemDto> dtoList = paged.getList().stream()
            .map(req -> {
                PermissionRequestListItemDto dto = new PermissionRequestListItemDto();
                dto.setRequestId(req.getRequestId());
                dto.setStatus(req.getStatus());
                dto.setRequestTime(req.getRequestTime());
                dto.setReason(req.getReason());
                dto.setRejectReason(req.getRejectReason());
                return dto;
            })
            .toList();

        PageData<PermissionRequestListItemDto> result = new PageData<>();
        result.setTotal(paged.getTotal());
        result.setPages(paged.getPages());
        result.setCurrent(paged.getCurrent());
        result.setSize(paged.getSize());
        result.setList(dtoList);
        return result;
    }
}
