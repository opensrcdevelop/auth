package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.entity.UserGroupMapping;
import cn.opensrcdevelop.auth.biz.mapper.UserGroupMappingMapper;
import cn.opensrcdevelop.auth.biz.service.UserGroupMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class UserGroupMappingServiceImpl extends ServiceImpl<UserGroupMappingMapper, UserGroupMapping> implements UserGroupMappingService {
}
