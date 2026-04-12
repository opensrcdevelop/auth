package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionAutoApprove;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionAutoApproveMapper;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionAutoApproveService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PermissionAutoApproveServiceImpl implements PermissionAutoApproveService {

    private final PermissionAutoApproveMapper permissionAutoApproveMapper;

    @Override
    public boolean isEnabled(String permissionId) {
        LambdaQueryWrapper<PermissionAutoApprove> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionAutoApprove::getPermissionId, permissionId)
                .eq(PermissionAutoApprove::getEnabled, Boolean.TRUE);
        return permissionAutoApproveMapper.selectCount(wrapper) > 0;
    }
}
