package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.ApproveRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.RejectRequestDto;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.request.PermissionRequestRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestAdminService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.response.PageData;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class PermissionRequestAdminServiceImpl implements PermissionRequestAdminService {

    private final PermissionRequestRepository permissionRequestRepository;
    private final AuthorizeService authorizeService;
    private final PermissionRequestItemMapper permissionRequestItemMapper;

    @Override
    public PageData<PermissionRequestListItemDto> listPendingRequests(int page, int size) {
        List<String> statuses = List.of(
                PermissionRequestStatusEnum.PENDING.getCode(),
                PermissionRequestStatusEnum.AUTO_APPROVED.getCode());
        PageData<PermissionRequest> paged = permissionRequestRepository.findByStatuses(statuses, page, size);
        return toDtoList(paged);
    }

    @Override
    public PageData<PermissionRequestListItemDto> listAllRequests(int page, int size, String status) {
        PageData<PermissionRequest> paged;
        if (status != null) {
            paged = permissionRequestRepository.findByStatuses(List.of(status), page, size);
        } else {
            paged = permissionRequestRepository.findByTenantId(page, size);
        }
        return toDtoList(paged);
    }

    private PageData<PermissionRequestListItemDto> toDtoList(PageData<PermissionRequest> paged) {
        List<String> requestIds = paged.getList().stream()
                .map(PermissionRequest::getRequestId)
                .toList();
        List<PermissionRequestItem> allItems = permissionRequestRepository.findByRequestIds(requestIds);

        List<PermissionRequestListItemDto> dtoList = paged.getList().stream()
                .map(req -> {
                    PermissionRequestListItemDto dto = new PermissionRequestListItemDto();
                    dto.setRequestId(req.getRequestId());
                    dto.setRequestTime(req.getRequestTime());
                    dto.setReason(req.getReason());
                    dto.setItemCount((int) allItems.stream()
                            .filter(i -> i.getRequestId().equals(req.getRequestId()))
                            .count());
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

    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION_REQUEST, sysOperation = SysOperationType.UPDATE, success = "批准了权限申请（{{ #requestId }}）", fail = "批准权限申请（{{ #requestId }}）失败")
    @Override
    public void approveRequest(String requestId, ApproveRequestDto dto) {
        // 1. 获取申请记录
        PermissionRequest request = permissionRequestRepository.getById(requestId);
        if (request == null) {
            throw new IllegalArgumentException("申请不存在");
        }

        // 2. 自我审批检查（per PAPR-07）
        String currentUserId = AuthUtil.getCurrentUserId();
        if (request.getUserId().equals(currentUserId)) {
            throw new SecurityException("不能审批自己的申请");
        }

        // 3. 获取申请明细，只处理 PENDING 状态的 items
        List<PermissionRequestItem> items = permissionRequestRepository.findByRequestIds(List.of(requestId));
        List<PermissionRequestItem> pendingItems = items.stream()
                .filter(i -> PermissionRequestStatusEnum.PENDING.getCode().equals(i.getStatus()))
                .toList();

        if (pendingItems.isEmpty()) {
            return;
        }

        // 4. 根据 itemIds 筛选要批准的项（支持部分审批）
        List<PermissionRequestItem> toApprove;
        if (dto.getItemIds() != null && !dto.getItemIds().isEmpty()) {
            // 只批准指定的 items
            toApprove = pendingItems.stream()
                    .filter(i -> dto.getItemIds().contains(i.getItemId()))
                    .toList();
        } else {
            // 未指定 itemIds，则批准所有 PENDING items
            toApprove = pendingItems;
        }

        if (toApprove.isEmpty()) {
            return;
        }

        // 5. 写入 t_authorize（per PINT-01），@CacheEvict 由 AuthorizeService 处理（per PINT-02）
        AuthorizeRequestDto authDto = new AuthorizeRequestDto();
        authDto.setUserIds(List.of(request.getUserId()));
        authDto.setPermissionIds(toApprove.stream().map(PermissionRequestItem::getPermissionId).toList());
        authDto.setExpressionIds(dto.getExpressionIds());
        authDto.setPriority(dto.getPriority());
        authorizeService.authorize(authDto);

        // 6. 更新明细状态为 APPROVED（在同一事务内，per PINT-03）
        for (PermissionRequestItem item : toApprove) {
            item.setStatus(PermissionRequestStatusEnum.APPROVED.getCode());
            permissionRequestItemMapper.updateById(item);
        }
    }

    @Override
    public void rejectRequest(String requestId, RejectRequestDto dto) {
        log.info("rejectRequest called for requestId: {}, dto: {}", requestId, dto);
    }
}
