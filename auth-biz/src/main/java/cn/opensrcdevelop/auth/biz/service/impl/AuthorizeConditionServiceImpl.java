package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.entity.AuthorizeCondition;
import cn.opensrcdevelop.auth.biz.mapper.AuthorizeConditionMapper;
import cn.opensrcdevelop.auth.biz.service.AuthorizeConditionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class AuthorizeConditionServiceImpl extends ServiceImpl<AuthorizeConditionMapper, AuthorizeCondition> implements AuthorizeConditionService {
}
