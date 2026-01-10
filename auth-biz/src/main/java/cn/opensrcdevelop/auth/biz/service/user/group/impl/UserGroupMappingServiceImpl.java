package cn.opensrcdevelop.auth.biz.service.user.group.impl;

import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroupMapping;
import cn.opensrcdevelop.auth.biz.mapper.user.group.UserGroupMappingMapper;
import cn.opensrcdevelop.auth.biz.service.user.group.UserGroupMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class UserGroupMappingServiceImpl extends ServiceImpl<UserGroupMappingMapper, UserGroupMapping>
        implements
            UserGroupMappingService {
}
