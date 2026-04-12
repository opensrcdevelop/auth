package cn.opensrcdevelop.auth.biz.repository.permission.request.impl;

import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.request.PermissionRequestRepository;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class PermissionRequestRepositoryImpl implements PermissionRequestRepository {

    private final PermissionRequestMapper permissionRequestMapper;
    private final PermissionRequestItemMapper permissionRequestItemMapper;

    @Override
    public PermissionRequest getById(String requestId) {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionRequest::getRequestId, requestId);
        return permissionRequestMapper.selectOne(wrapper);
    }

    @Override
    public List<PermissionRequest> findByUserId(String userId) {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionRequest::getUserId, userId)
                .orderByDesc(PermissionRequest::getRequestTime);
        return permissionRequestMapper.selectList(wrapper);
    }

    @Override
    public List<PermissionRequest> findByStatus(String status) {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionRequest::getStatus, status)
                .orderByDesc(PermissionRequest::getRequestTime);
        return permissionRequestMapper.selectList(wrapper);
    }

    @Override
    public PageData<PermissionRequest> findByTenantId(int page, int pageSize) {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.orderByDesc(PermissionRequest::getRequestTime);
        IPage<PermissionRequest> iPage = permissionRequestMapper.selectPage(new Page<>(page, pageSize), wrapper);
        PageData<PermissionRequest> pageData = new PageData<>();
        pageData.setTotal(iPage.getTotal());
        pageData.setPages(iPage.getPages());
        pageData.setCurrent(iPage.getCurrent());
        pageData.setSize(iPage.getSize());
        pageData.setList(iPage.getRecords());
        return pageData;
    }

    @Override
    public PageData<PermissionRequest> findByStatus(String status, int page, int pageSize) {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionRequest::getStatus, status)
                .orderByDesc(PermissionRequest::getRequestTime);
        IPage<PermissionRequest> iPage = permissionRequestMapper.selectPage(new Page<>(page, pageSize), wrapper);
        PageData<PermissionRequest> pageData = new PageData<>();
        pageData.setTotal(iPage.getTotal());
        pageData.setPages(iPage.getPages());
        pageData.setCurrent(iPage.getCurrent());
        pageData.setSize(iPage.getSize());
        pageData.setList(iPage.getRecords());
        return pageData;
    }

    @Override
    public boolean hasActivePendingRequest(String userId, java.util.List<String> permissionIds) {
        // 步骤 1：查询该用户所有 PENDING 或 AUTO_APPROVED 状态的申请ID
        LambdaQueryWrapper<PermissionRequest> requestWrapper =
                new LambdaQueryWrapper<>();
        requestWrapper.eq(PermissionRequest::getUserId, userId)
                .in(PermissionRequest::getStatus,
                        java.util.List.of(
                                PermissionRequestStatusEnum.PENDING.getCode(),
                                PermissionRequestStatusEnum.AUTO_APPROVED.getCode()));
        java.util.List<String> activeRequestIds = permissionRequestMapper.selectList(requestWrapper)
                .stream()
                .map(PermissionRequest::getRequestId)
                .toList();

        if (activeRequestIds.isEmpty()) {
            return false;
        }

        // 步骤 2：检查这些申请中是否包含待检查的权限
        LambdaQueryWrapper<PermissionRequestItem> itemWrapper =
                new LambdaQueryWrapper<>();
        itemWrapper.in(
                        PermissionRequestItem::getRequestId,
                        activeRequestIds)
                .in(PermissionRequestItem::getPermissionId,
                        permissionIds);
        return permissionRequestItemMapper.selectCount(itemWrapper) > 0;
    }
}
