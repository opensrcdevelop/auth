package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.entity.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.mapper.UserAttrMappingMapper;
import cn.opensrcdevelop.auth.biz.service.UserAttrMappingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class UserAttrMappingServiceImpl extends ServiceImpl<UserAttrMappingMapper, UserAttrMapping> implements UserAttrMappingService {
}
