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
        // 状态在 item 上，需要先通过 item 查询出 requestIds，再查 request
        LambdaQueryWrapper<PermissionRequestItem> itemWrapper = new LambdaQueryWrapper<>();
        itemWrapper.eq(PermissionRequestItem::getStatus, status);
        List<PermissionRequestItem> items = permissionRequestItemMapper.selectList(itemWrapper);
        if (items.isEmpty()) {
            return List.of();
        }
        List<String> requestIds = items.stream()
                .map(PermissionRequestItem::getRequestId)
                .distinct()
                .toList();
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(PermissionRequest::getRequestId, requestIds)
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
        // 状态在 item 上，需要先通过 item 查询出 requestIds，再查 request
        LambdaQueryWrapper<PermissionRequestItem> itemWrapper = new LambdaQueryWrapper<>();
        itemWrapper.eq(PermissionRequestItem::getStatus, status);
        List<PermissionRequestItem> items = permissionRequestItemMapper.selectList(itemWrapper);
        if (items.isEmpty()) {
            PageData<PermissionRequest> emptyPage = new PageData<>();
            emptyPage.setTotal(0L);
            emptyPage.setPages(0L);
            emptyPage.setCurrent((long) page);
            emptyPage.setSize((long) pageSize);
            emptyPage.setList(List.of());
            return emptyPage;
        }
        List<String> requestIds = items.stream()
                .map(PermissionRequestItem::getRequestId)
                .distinct()
                .toList();
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(PermissionRequest::getRequestId, requestIds)
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
        // 步骤 1：查询该用户所有申请ID
        LambdaQueryWrapper<PermissionRequest> requestWrapper = new LambdaQueryWrapper<>();
        requestWrapper.eq(PermissionRequest::getUserId, userId);
        java.util.List<String> requestIds = permissionRequestMapper.selectList(requestWrapper)
                .stream()
                .map(PermissionRequest::getRequestId)
                .toList();

        if (requestIds.isEmpty()) {
            return false;
        }

        // 步骤 2：检查这些申请中是否有 PENDING 或 AUTO_APPROVED 状态的指定权限
        LambdaQueryWrapper<PermissionRequestItem> itemWrapper = new LambdaQueryWrapper<>();
        itemWrapper.in(PermissionRequestItem::getRequestId, requestIds)
                .in(PermissionRequestItem::getPermissionId, permissionIds)
                .in(PermissionRequestItem::getStatus,
                        java.util.List.of(
                                PermissionRequestStatusEnum.PENDING.getCode(),
                                PermissionRequestStatusEnum.AUTO_APPROVED.getCode()));
        return permissionRequestItemMapper.selectCount(itemWrapper) > 0;
    }

    @Override
    public PageData<PermissionRequest> findByUserIdPaged(String userId, int page, int pageSize) {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionRequest::getUserId, userId)
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
    public PageData<PermissionRequest> findByStatuses(List<String> statuses, int page, int pageSize) {
        // 状态在 item 上，需要先通过 item 查询出 requestIds，再查 request
        LambdaQueryWrapper<PermissionRequestItem> itemWrapper = new LambdaQueryWrapper<>();
        itemWrapper.in(PermissionRequestItem::getStatus, statuses);
        List<PermissionRequestItem> items = permissionRequestItemMapper.selectList(itemWrapper);
        if (items.isEmpty()) {
            PageData<PermissionRequest> emptyPage = new PageData<>();
            emptyPage.setTotal(0L);
            emptyPage.setPages(0L);
            emptyPage.setCurrent((long) page);
            emptyPage.setSize((long) pageSize);
            emptyPage.setList(List.of());
            return emptyPage;
        }
        List<String> requestIds = items.stream()
                .map(PermissionRequestItem::getRequestId)
                .distinct()
                .toList();

        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(PermissionRequest::getRequestId, requestIds)
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
    public List<PermissionRequestItem> findByRequestIds(List<String> requestIds) {
        if (requestIds == null || requestIds.isEmpty()) {
            return List.of();
        }
        LambdaQueryWrapper<PermissionRequestItem> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(PermissionRequestItem::getRequestId, requestIds);
        return permissionRequestItemMapper.selectList(wrapper);
    }
}
