package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.constants.AuthorizeTypeEnum;
import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestApproveRequestDto;
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
import cn.opensrcdevelop.auth.biz.service.permission.impl.PermissionServiceImpl;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestItemService;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.Strings;
import org.redisson.api.RLock;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class PermissionRequestServiceImpl extends ServiceImpl<PermissionRequestMapper, PermissionRequest>
        implements
            PermissionRequestService {

    private static final String PERMISSION_APPROVE_LOCK_KEY = "permission_approve_lock:%s";

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
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.PERMISSION_REQUEST, userOperation = UserOperationType.REQUEST_PERMISSION, success = "提交了权限申请（{{ @linkGen.toLink(#requestId, T(ResourceType).PERMISSION_REQUEST) }}）, 申请 {{ #count }} 条权限", fail = "提交权限申请失败，申请 {{ #count }} 条权限")
    @Transactional
    @Override
    public void submitRequest(PermissionRequestCreateDto dto) {
        String userId = AuthUtil.getCurrentUserId();
        List<String> permissionIds = dto.getPermissionIds();

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
                item.setApproveTime(LocalDateTime.now());
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

        AuditContext.setSpelVariable("requestId", requestId);
        AuditContext.setSpelVariable("count", permissionIds.size());
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
     * @param pendingOnly
     *            只查看待审批的权限申请
     * @return 权限申请列表分页数据
     */
    @Override
    public PageData<PermissionRequestResponseDto> listRequests(List<String> userIds, String usernameSearchKeyword,
            int page, int size, Boolean pendingOnly) {
        // 1. 分页查询权限申请记录（包含 items）
        Page<PermissionRequest> pageParam = new Page<>(page, size);
        permissionRequestRepository.searchPermissionRequests(pageParam, userIds, usernameSearchKeyword, pendingOnly);

        // 2. 组装响应数据并计算统计指标
        List<PermissionRequestResponseDto> dtoList = pageParam.getRecords().stream()
                .map(request -> {
                    PermissionRequestResponseDto dto = new PermissionRequestResponseDto();
                    dto.setRequestId(request.getRequestId());
                    dto.setUserId(request.getUserId());
                    dto.setUsername(request.getUsername());
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

        // 2.2 如果只查看待审批，则过滤掉没有待审批项的申请
        if (Boolean.TRUE.equals(pendingOnly)) {
            dtoList = dtoList.stream()
                    .filter(dto -> dto.getPendingCount() != null && dto.getPendingCount() > 0)
                    .toList();
        }

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
                    dto.setId(item.getItemId());
                    dto.setPermissionId(item.getPermissionId());
                    dto.setStatus(item.getStatus());
                    dto.setRejectReason(item.getRejectReason());
                    dto.setApproverId(item.getApproverId());
                    dto.setApproverUsername(item.getApproverUsername());
                    dto.setApproveTime(item.getApproveTime());

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
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.PERMISSION_REQUEST, userOperation = UserOperationType.CANCEL_PERMISSION_REQUEST, success = "取消了权限申请（{{ @linkGen.toLink(#requestId, T(ResourceType).PERMISSION_REQUEST) }}）", fail = "取消权限申请失败")
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
            RedisUtil.delete(CacheConstants.CACHE_CURRENT_USER_PERMISSIONS + "::"
                    + ((PermissionServiceImpl) permissionService).generateCurrentUserPermissionsCacheKey());
        }

        // 5. 删除权限申请和明细
        super.removeById(requestId);
        permissionRequestItemService.remove(Wrappers.<PermissionRequestItem>lambdaQuery()
                .eq(PermissionRequestItem::getRequestId, requestId));
    }

    /**
     * 审批权限申请
     *
     * @param requestDto
     *            审批权限申请请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION_REQUEST, sysOperation = SysOperationType.UPDATE, success = "审批了权限申请（{{ @linkGen.toLink(#requestDto.requestId, T(ResourceType).PERMISSION_REQUEST) }}），操作：{{ #requestDto.approve ? '批准' : '拒绝' }}", fail = "审批权限申请（{{ @linkGen.toLink(#requestDto.requestId, T(ResourceType).PERMISSION_REQUEST) }}）失败，操作：{{ #requestDto.approve ? '批准' : '拒绝' }}")
    @Override
    public void approveRequest(PermissionRequestApproveRequestDto requestDto) {

        // 1. 检查权限申请是否存在
        String requestId = requestDto.getRequestId();
        PermissionRequest permissionRequest = super.getById(requestId);
        boolean exists = permissionRequest != null;
        if (!exists) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1002);
        }

        // 2. 检查审批人与申请人是否相同
        if (Strings.CI.equals(permissionRequest.getUserId(), AuthUtil.getCurrentUserId())) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1006);
        }

        // 3. 检查是否已在审批中
        if (RedisUtil.getLock(PERMISSION_APPROVE_LOCK_KEY.formatted(requestId)).isLocked()) {
            throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1005);
        }

        RLock lock = RedisUtil.getLock(PERMISSION_APPROVE_LOCK_KEY.formatted(requestId));
        try {
            if (!lock.tryLock()) {
                throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1005);
            }

            // 4. 检查待审批的明细
            boolean hasItems = CollectionUtils.isNotEmpty(requestDto.getItemIds());
            List<PermissionRequestItem> pendingPermissionRequestItems = permissionRequestItemService
                    .list(Wrappers.<PermissionRequestItem>lambdaQuery()
                            .select(PermissionRequestItem::getItemId, PermissionRequestItem::getPermissionId)
                            .eq(PermissionRequestItem::getRequestId, requestId)
                            .eq(PermissionRequestItem::getStatus, PermissionRequestStatusEnum.PENDING.getCode()));
            List<String> pendingItemIds = CommonUtil
                    .stream(pendingPermissionRequestItems)
                    .map(PermissionRequestItem::getItemId).toList();
            if (CollectionUtils.isEmpty(pendingItemIds)) {
                throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1004);
            }

            List<String> filteredPendingItemIds = CommonUtil.stream(requestDto.getItemIds())
                    .filter(pendingItemIds::contains).toList();
            if (hasItems && CollectionUtils.isEmpty(filteredPendingItemIds)) {
                throw new BizException(MessageConstants.PERMISSION_REQUEST_MSG_1004);
            }

            // 5. 更新权限申请明细状态
            boolean approve = Boolean.TRUE.equals(requestDto.getApprove());
            permissionRequestItemService.update(Wrappers.<PermissionRequestItem>lambdaUpdate()
                    .set(PermissionRequestItem::getStatus,
                            approve
                                    ? PermissionRequestStatusEnum.APPROVED.getCode()
                                    : PermissionRequestStatusEnum.REJECTED.getCode())
                    .set(PermissionRequestItem::getApproverId, AuthUtil.getCurrentUserId())
                    .set(PermissionRequestItem::getApproveTime, LocalDateTime.now())
                    .set(PermissionRequestItem::getRejectReason, approve ? null : requestDto.getRejectReason())
                    .in(PermissionRequestItem::getItemId, hasItems ? filteredPendingItemIds : pendingItemIds));

            // 6. 添加授权记录
            if (approve) {
                List<String> itemIds = hasItems ? filteredPendingItemIds : pendingItemIds;
                List<String> permissionIds = CommonUtil.stream(pendingPermissionRequestItems)
                        .filter(item -> itemIds.contains(item.getItemId()))
                        .map(PermissionRequestItem::getPermissionId)
                        .toList();

                AuthorizeRequestDto authorizeRequestDto = new AuthorizeRequestDto();
                authorizeRequestDto.setUserIds(List.of(permissionRequest.getUserId()));
                authorizeRequestDto.setPermissionIds(permissionIds);
                authorizeRequestDto.setPriority(0);
                authorizeRequestDto.setExpressionIds(requestDto.getExpressionIds());
                authorizeService.authorize(authorizeRequestDto, AuthorizeTypeEnum.ADMINISTRATOR_APPROVE);
            }
        } finally {
            if (lock.isLocked()) {
                lock.unlock();
            }
        }
    }

    /**
     * 获取权限申请详情
     *
     * @param requestId
     *            权限申请ID
     * @return 权限申请详情
     */
    @Override
    public PermissionRequestResponseDto detail(String requestId) {
        PermissionRequestResponseDto responseDto = new PermissionRequestResponseDto();

        // 1. 获取权限申请
        PermissionRequest permissionRequest = permissionRequestRepository.getById(requestId);
        if (Objects.isNull(permissionRequest)) {
            return responseDto;
        }
        responseDto.setRequestId(permissionRequest.getRequestId());
        responseDto.setUserId(permissionRequest.getUserId());
        responseDto.setUsername(permissionRequest.getUsername());
        responseDto.setRequestTime(permissionRequest.getRequestTime());
        responseDto.setReason(permissionRequest.getReason());

        // 2. 获取权限申请明细
        List<PermissionRequestItemResponseDto> items = listRequestItems(null, requestId);
        responseDto.setItems(items);

        // 2.1 计算统计指标
        if (CollectionUtils.isNotEmpty(items)) {
            responseDto.setPendingCount(items.stream()
                    .filter(item -> PermissionRequestStatusEnum.PENDING.getCode().equals(item.getStatus()))
                    .count());
            responseDto.setApprovedCount(items.stream()
                    .filter(item -> PermissionRequestStatusEnum.APPROVED.getCode().equals(item.getStatus()))
                    .count());
            responseDto.setAutoApproveCount(items.stream()
                    .filter(item -> PermissionRequestStatusEnum.AUTO_APPROVED.getCode()
                            .equals(item.getStatus()))
                    .count());
            responseDto.setRejectedCount(items.stream()
                    .filter(item -> PermissionRequestStatusEnum.REJECTED.getCode().equals(item.getStatus()))
                    .count());
            responseDto.setTotalCount((long) items.size());
        } else {
            responseDto.setPendingCount(0L);
            responseDto.setApprovedCount(0L);
            responseDto.setAutoApproveCount(0L);
            responseDto.setRejectedCount(0L);
            responseDto.setTotalCount(0L);
        }

        return responseDto;
    }
}
