package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.entity.RoleMapping;
import cn.opensrcdevelop.auth.biz.mapper.RoleMappingMapper;
import cn.opensrcdevelop.auth.biz.service.RoleMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class RoleMappingServiceImpl extends ServiceImpl<RoleMappingMapper, RoleMapping> implements RoleMappingService {
}
