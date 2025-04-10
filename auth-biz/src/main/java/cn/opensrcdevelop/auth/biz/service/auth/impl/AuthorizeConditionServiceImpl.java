package cn.opensrcdevelop.auth.biz.service.auth.impl;

import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeCondition;
import cn.opensrcdevelop.auth.biz.mapper.auth.AuthorizeConditionMapper;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeConditionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class AuthorizeConditionServiceImpl extends ServiceImpl<AuthorizeConditionMapper, AuthorizeCondition> implements AuthorizeConditionService {
}
