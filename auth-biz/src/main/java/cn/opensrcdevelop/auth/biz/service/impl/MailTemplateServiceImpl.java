package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateParamResponseDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.entity.MailTemplate;
import cn.opensrcdevelop.auth.biz.mapper.MailTemplateMapper;
import cn.opensrcdevelop.auth.biz.service.MailTemplateService;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.core.type.TypeReference;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
public class MailTemplateServiceImpl extends ServiceImpl<MailTemplateMapper, MailTemplate> implements MailTemplateService {

    /**
     * 获取邮件模版列表
     *
     * @return 邮件模版列表
     */
    @Override
    public List<MailTemplateResponseDto> templateList() {
        // 1. 查询数据库
        List<MailTemplate> mailTemplateList = super.list();

        // 2. 响应编辑
        return CommonUtil.stream(mailTemplateList).map(mailTemplate ->
                MailTemplateResponseDto.builder()
                        .id(mailTemplate.getTemplateId())
                        .name(mailTemplate.getTemplateName())
                        .desc(mailTemplate.getDescription()).build()
        ).toList();
    }

    /**
     * 获取邮件模版详情
     *
     * @param templateId 邮件模版ID
     * @return 邮件模版详情
     */
    @Override
    public MailTemplateResponseDto detail(String templateId) {
        // 1. 查询数据库
        MailTemplate mailTemplate = super.getById(templateId);

        // 2. 响应编辑
        MailTemplateResponseDto responseDto = null;
        if (Objects.nonNull(mailTemplate)) {
            // 2.1 处理模版参数
            List<MailTemplateParamResponseDto> params = null;
            if (StringUtils.isNotBlank(mailTemplate.getTemplateParameters())) {
                Map<String, String> paramsMap = CommonUtil.deserializeObject(mailTemplate.getTemplateParameters(), new TypeReference<Map<String, String>>() {});
                params = CommonUtil.stream(paramsMap.entrySet()).map(entry ->
                        MailTemplateParamResponseDto.builder()
                                .key(entry.getKey())
                                .value(entry.getValue()).build())
                        .toList();
            }

            responseDto = MailTemplateResponseDto.builder()
                    .id(mailTemplate.getTemplateId())
                    .name(mailTemplate.getTemplateName())
                    .subject(mailTemplate.getSubject())
                    .sender(mailTemplate.getSender())
                    .content(mailTemplate.getTemplateContent())
                    .parameters(params)
                    .build();
        }
        return responseDto;
    }

    /**
     * 更新邮件模版
     *
     * @param requestDto 更新邮件模版请求
     */
    @Transactional
    @Override
    public void update(MailTemplateRequestDto requestDto) {
        // 1. 编辑更新实体
        MailTemplate updateMailTemplate = new MailTemplate();
        updateMailTemplate.setTemplateId(requestDto.getId());
        updateMailTemplate.setSubject(requestDto.getSubject());
        updateMailTemplate.setSender(requestDto.getSender());
        updateMailTemplate.setTemplateContent(requestDto.getContent());

        // 2. 数据库操作
        super.updateById(updateMailTemplate);

        // 3. 删除缓存
        MailTemplate mailTemplate = super.getById(updateMailTemplate.getTemplateId());
        RedisUtil.delete(getCacheKey(mailTemplate.getTemplateCode()));
    }

    /**
     * 获取邮件模版
     *
     * @param code 模版标识
     * @return 邮件模版
     */
    @Override
    public MailTemplate getByCode(String code) {
        // 1. 从缓存获取
        MailTemplate mailTemplate = RedisUtil.get(getCacheKey(code), MailTemplate.class);

        if (Objects.isNull(mailTemplate)) {
            // 2. 从数据库获取
            mailTemplate = super.getOne(Wrappers.<MailTemplate>lambdaQuery()
                    .select(MailTemplate::getSubject, MailTemplate::getSender, MailTemplate::getTemplateContent)
                    .eq(MailTemplate::getTemplateCode, code));
            if (Objects.nonNull(mailTemplate)) {
                RedisUtil.set(getCacheKey(code), mailTemplate);
            }
        }
        return mailTemplate;
    }

    private String getCacheKey(String code) {
        return CacheConstants.CACHE_MAIL_TEMPLATE
                + ":"
                + TenantContextHolder.getTenantContext().getTenantCode()
                + ":"
                + code;
    }
}
