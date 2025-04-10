package cn.opensrcdevelop.auth.biz.service.system.password.impl;

import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicyMapping;
import cn.opensrcdevelop.auth.biz.mapper.system.password.PasswordPolicyMappingMapper;
import cn.opensrcdevelop.auth.biz.service.system.password.PasswordPolicyMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class PasswordPolicyMappingServiceImpl extends ServiceImpl<PasswordPolicyMappingMapper, PasswordPolicyMapping> implements PasswordPolicyMappingService {
}
