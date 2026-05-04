package cn.opensrcdevelop.auth.biz.repository.permission.impl;

import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionRequestMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.PermissionRequestRepository;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class PermissionRequestRepositoryImpl implements PermissionRequestRepository {

    private final PermissionRequestMapper permissionRequestMapper;
    private final PermissionRequestItemMapper permissionRequestItemMapper;

    /**
     * 检查用户是否有待处理的权限申请
     *
     * @param userId
     *            用户ID
     * @param permissionIds
     *            权限ID列表
     * @return 是否有待处理的权限申请
     */
    @Override
    public boolean hasActivePendingRequest(String userId, List<String> permissionIds) {
        LambdaQueryWrapper<PermissionRequest> requestWrapper = new LambdaQueryWrapper<>();
        requestWrapper.eq(PermissionRequest::getUserId, userId);
        List<String> requestIds = permissionRequestMapper.selectList(requestWrapper)
                .stream()
                .map(PermissionRequest::getRequestId)
                .toList();

        if (requestIds.isEmpty()) {
            return false;
        }

        LambdaQueryWrapper<PermissionRequestItem> itemWrapper = new LambdaQueryWrapper<>();
        itemWrapper.in(PermissionRequestItem::getRequestId, requestIds)
                .in(PermissionRequestItem::getPermissionId, permissionIds)
                .in(PermissionRequestItem::getStatus, List.of(PermissionRequestStatusEnum.PENDING.name()));
        return permissionRequestItemMapper.selectCount(itemWrapper) > 0;
    }

    /**
     * 检索用户权限申请
     *
     * @param userIds
     *            用户ID列表
     * @param pendingOnly
     *            只查看待审批的权限申请
     */
    @Override
    public void searchPermissionRequests(IPage<PermissionRequest> page, List<String> userIds,
            String usernameSearchKeyword, Boolean pendingOnly) {
        permissionRequestMapper.searchPermissionRequests(page, userIds, usernameSearchKeyword, pendingOnly);
    }

    /**
     * 获取权限申请明细列表
     *
     * @param requestId
     *            权限申请ID
     * @return 权限申请明细列表
     */
    @Override
    public List<PermissionRequestItem> getPermissionRequestItems(String userId, String requestId) {
        return permissionRequestItemMapper.getPermissionRequestItemsByRequestIdAndUserId(userId, requestId);
    }

    /**
     * 根据ID获取权限申请
     *
     * @param requestId
     *            权限申请ID
     * @return 权限申请
     */
    @Override
    public PermissionRequest getById(String requestId) {
        return permissionRequestMapper.getById(requestId);
    }
}
