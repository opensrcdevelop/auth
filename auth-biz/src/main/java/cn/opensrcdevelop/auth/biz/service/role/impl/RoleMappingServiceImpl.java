package cn.opensrcdevelop.auth.biz.service.role.impl;

import cn.opensrcdevelop.auth.biz.entity.role.RoleMapping;
import cn.opensrcdevelop.auth.biz.mapper.role.RoleMappingMapper;
import cn.opensrcdevelop.auth.biz.service.role.RoleMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class RoleMappingServiceImpl extends ServiceImpl<RoleMappingMapper, RoleMapping> implements RoleMappingService {
}
