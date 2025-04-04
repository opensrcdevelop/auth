package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.entity.PasswordPolicyMapping;
import cn.opensrcdevelop.auth.biz.mapper.PasswordPolicyMappingMapper;
import cn.opensrcdevelop.auth.biz.service.PasswordPolicyMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class PasswordPolicyMappingServiceImpl extends ServiceImpl<PasswordPolicyMappingMapper, PasswordPolicyMapping> implements PasswordPolicyMappingService {
}
