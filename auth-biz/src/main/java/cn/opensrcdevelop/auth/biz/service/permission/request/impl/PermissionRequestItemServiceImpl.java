package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestItemService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class PermissionRequestItemServiceImpl extends ServiceImpl<PermissionRequestItemMapper, PermissionRequestItem>
        implements
            PermissionRequestItemService {
}
