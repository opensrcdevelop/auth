package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.AuthorizeTypeEnum;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestItemResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionRequestMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.PermissionRepository;
import cn.opensrcdevelop.auth.biz.repository.permission.PermissionRequestRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestItemService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class PermissionRequestServiceImpl extends ServiceImpl<PermissionRequestMapper, PermissionRequest>
        implements
            PermissionRequestService {

    private final AuthorizeService authorizeService;
    private final PermissionRequestRepository permissionRequestRepository;
    private final PermissionService permissionService;
    private final PermissionRepository permissionRepository;
    private final PermissionRequestItemService permissionRequestItemService;

    /**
     * 提交权限申请
     *
     * @param dto
     *            申请请求
     */
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.PERMISSION_REQUEST, sysOperation = SysOperationType.CREATE, success = "提交了权限申请（{{ @linkGen.toLink(#requestId, T(ResourceType).PERMISSION_REQUEST) }}）, 申请 {{ #count }} 条权限", fail = "提交权限申请失败，申请 {{ #count }} 条权限")
    @Transactional
    @Override
    public void submitRequest(PermissionRequestCreateDto dto) {
        String userId = AuthUtil.getCurrentUserId();
        List<String> permissionIds = dto.getPermissionIds();
        AuditContext.setSpelVariable("count", permissionIds.size());

        // 1. 获取当前用户权限
        List<String> userPermissionIds = CommonUtil.stream(permissionService.getCurrentUserPermissions())
                .map(PermissionResponseDto::getPermissionId).toList();

        // 2. 已拥有权限和重复申请检查
        if (CommonUtil.stream(permissionIds).anyMatch(userPermissionIds::contains) ||
                permissionRequestRepository.hasActivePendingRequest(userId, permissionIds)) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1000);
        }

        // 3. 获取可申请的权限
        List<Permission> allowApplyPermissions = permissionRepository.getAllowApplyPermissions();
        List<String> allowApplyPermissionIds = CommonUtil.stream(allowApplyPermissions).map(Permission::getPermissionId)
                .toList();
        List<String> autoApprovePermissionIds = CommonUtil.stream(allowApplyPermissions)
                .filter(Permission::getAutoApprove).map(Permission::getPermissionId).toList();
        // 3.1 移除不可申请的权限
        List<String> filteredPermissionIds = CommonUtil.stream(permissionIds).filter(allowApplyPermissionIds::contains)
                .toList();
        if (CollectionUtils.isEmpty(filteredPermissionIds)) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1001);
        }

        // 4. 添加申请记录
        String requestId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("requestId", requestId);

        PermissionRequest permissionRequest = new PermissionRequest();
        permissionRequest.setRequestId(requestId);
        permissionRequest.setRequestTime(LocalDateTime.now());
        permissionRequest.setUserId(userId);
        permissionRequest.setReason(dto.getReason());
        super.save(permissionRequest);

        // 4.1 添加申请子项
        List<String> autoApprovedPermissionIds = new ArrayList<>();
        List<PermissionRequestItem> itemList = CommonUtil.stream(filteredPermissionIds).map(id -> {
            PermissionRequestItem item = new PermissionRequestItem();
            item.setRequestId(requestId);
            item.setItemId(CommonUtil.getUUIDV7String());
            item.setPermissionId(id);

            if (autoApprovePermissionIds.contains(id)) {
                autoApprovedPermissionIds.add(id);
                item.setStatus(PermissionRequestStatusEnum.AUTO_APPROVED.getCode());
            } else {
                item.setStatus(PermissionRequestStatusEnum.PENDING.getCode());
            }
            return item;
        }).toList();
        permissionRequestItemService.saveBatch(itemList);

        // 4. 自动批准的权限添加授权记录
        if (CollectionUtils.isNotEmpty(autoApprovedPermissionIds)) {
            AuthorizeRequestDto authorizeRequestDto = new AuthorizeRequestDto();
            authorizeRequestDto.setUserIds(List.of(userId));
            authorizeRequestDto.setPriority(0);
            authorizeRequestDto.setPermissionIds(autoApprovedPermissionIds);
            authorizeService.authorize(authorizeRequestDto, AuthorizeTypeEnum.AUTO_APPROVE);
        }
    }

    /**
     * 获取用户权限申请列表
     *
     * @param userIds
     *            用户ID列表
     * @param usernameSearchKeyword
     *            用户名搜索关键词
     * @param page
     *            页码
     * @param size
     *            每页数量
     * @return 权限申请列表分页数据
     */
    @Override
    public PageData<PermissionRequestResponseDto> listRequests(List<String> userIds, String usernameSearchKeyword,
            int page, int size) {
        // 1. 分页查询权限申请记录（包含 items）
        Page<PermissionRequest> pageParam = new Page<>(page, size);
        permissionRequestRepository.searchPermissionRequests(pageParam, userIds, usernameSearchKeyword);

        // 2. 组装响应数据并计算统计指标
        List<PermissionRequestResponseDto> dtoList = pageParam.getRecords().stream()
                .map(request -> {
                    PermissionRequestResponseDto dto = new PermissionRequestResponseDto();
                    dto.setRequestId(request.getRequestId());
                    dto.setUserId(request.getUserId());
                    dto.setReason(request.getReason());
                    dto.setRequestTime(request.getRequestTime());

                    // 2.1 获取该申请的所有明细并计算统计指标
                    List<PermissionRequestItem> items = request.getItems();
                    if (CollectionUtils.isNotEmpty(items)) {
                        dto.setPendingCount(items.stream()
                                .filter(item -> PermissionRequestStatusEnum.PENDING.getCode().equals(item.getStatus()))
                                .count());
                        dto.setApprovedCount(items.stream()
                                .filter(item -> PermissionRequestStatusEnum.APPROVED.getCode().equals(item.getStatus()))
                                .count());
                        dto.setAutoApproveCount(items.stream()
                                .filter(item -> PermissionRequestStatusEnum.AUTO_APPROVED.getCode()
                                        .equals(item.getStatus()))
                                .count());
                        dto.setRejectedCount(items.stream()
                                .filter(item -> PermissionRequestStatusEnum.REJECTED.getCode().equals(item.getStatus()))
                                .count());
                        dto.setTotalCount((long) items.size());
                    } else {
                        dto.setPendingCount(0L);
                        dto.setApprovedCount(0L);
                        dto.setAutoApproveCount(0L);
                        dto.setRejectedCount(0L);
                        dto.setTotalCount(0L);
                    }

                    return dto;
                })
                .toList();

        // 3. 构建分页响应
        PageData<PermissionRequestResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageParam.getTotal());
        pageData.setPages(pageParam.getPages());
        pageData.setCurrent(pageParam.getCurrent());
        pageData.setSize(pageParam.getSize());
        pageData.setList(dtoList);
        return pageData;
    }

    /**
     * 获取权限申请明细列表
     *
     * @param userId
     *            用户ID
     * @param requestId
     *            权限申请ID
     * @return 权限申请明细列表
     */
    @Override
    public List<PermissionRequestItemResponseDto> listRequestItems(String userId, String requestId) {
        // 1. 查询权限申请明细
        List<PermissionRequestItem> items = permissionRequestRepository.getPermissionRequestItems(userId, requestId);

        // 2. 组装响应数据
        return items.stream()
                .map(item -> {
                    PermissionRequestItemResponseDto dto = new PermissionRequestItemResponseDto();
                    dto.setPermissionId(item.getPermissionId());
                    dto.setStatus(item.getStatus());
                    dto.setRejectReason(item.getRejectReason());
                    dto.setApproverUsername(item.getApproverUsername());

                    if (Objects.nonNull(item.getPermission())) {
                        dto.setPermissionName(item.getPermission().getPermissionName());
                        Resource resource = item.getPermission().getResource();
                        if (Objects.nonNull(resource)) {
                            dto.setResourceId(resource.getResourceId());
                            dto.setResourceName(resource.getResourceName());
                            if (Objects.nonNull(resource.getResourceGroup())) {
                                dto.setResourceGroupId(resource.getResourceGroup().getResourceGroupId());
                                dto.setResourceGroupName(resource.getResourceGroup().getResourceGroupName());
                            }
                        }
                    }
                    return dto;
                })
                .toList();
    }

    /**
     * 用户取消权限申请
     *
     * @param requestId
     *            权限申请ID
     */
    @Transactional
    @Override
    public void cancelRequest(String requestId) {
        // 1. 获取当前用户 ID
        String userId = AuthUtil.getCurrentUserId();

        // 2. 检查权限申请是否存在
        boolean exists = super.exists(Wrappers.<PermissionRequest>lambdaQuery()
                .eq(PermissionRequest::getUserId, userId)
                .eq(PermissionRequest::getRequestId, requestId));
        if (!exists) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1002);
        }

        // 3. 检查权限申请状态是否为待处理
        boolean alreadyApprovedOrRejected = permissionRequestItemService
                .exists(Wrappers.<PermissionRequestItem>lambdaQuery()
                        .eq(PermissionRequestItem::getRequestId, requestId)
                        .in(PermissionRequestItem::getStatus, List.of(PermissionRequestStatusEnum.APPROVED.getCode(),
                                PermissionRequestStatusEnum.REJECTED.getCode())));
        if (alreadyApprovedOrRejected) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1003);
        }

        // 4. 删除自动批准的授权
        List<String> autoApprovedPermissionIds = CommonUtil
                .stream(permissionRequestItemService.list(Wrappers.<PermissionRequestItem>lambdaQuery()
                        .eq(PermissionRequestItem::getRequestId, requestId)
                        .eq(PermissionRequestItem::getStatus, PermissionRequestStatusEnum.AUTO_APPROVED.name())))
                .map(PermissionRequestItem::getPermissionId)
                .toList();
        if (CollectionUtils.isNotEmpty(autoApprovedPermissionIds)) {
            authorizeService.remove(Wrappers.<AuthorizeRecord>lambdaQuery()
                    .eq(AuthorizeRecord::getUserId, userId)
                    .eq(AuthorizeRecord::getType, AuthorizeTypeEnum.AUTO_APPROVE.getType())
                    .in(AuthorizeRecord::getPermissionId, autoApprovedPermissionIds));
        }

        // 5. 删除权限申请和明细
        super.removeById(requestId);
        permissionRequestItemService.remove(Wrappers.<PermissionRequestItem>lambdaQuery()
                .eq(PermissionRequestItem::getRequestId, requestId));
    }
}
