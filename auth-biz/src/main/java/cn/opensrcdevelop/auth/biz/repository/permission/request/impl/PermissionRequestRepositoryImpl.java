package cn.opensrcdevelop.auth.biz.repository.permission.request.impl;

import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
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
}
