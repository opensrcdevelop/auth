package cn.opensrcdevelop.auth.biz.service.user.attr.impl;

import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.UserAttrMappingMapper;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class UserAttrMappingServiceImpl extends ServiceImpl<UserAttrMappingMapper, UserAttrMapping> implements UserAttrMappingService {
}
