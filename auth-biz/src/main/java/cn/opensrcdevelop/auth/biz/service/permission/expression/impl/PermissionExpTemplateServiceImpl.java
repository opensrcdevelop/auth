package cn.opensrcdevelop.auth.biz.service.permission.expression.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PermissionExpTemplateParamType;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamConfigDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionExp;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionExpTemplate;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionExpTemplateMapper;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpService;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpTemplateService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.core.type.TypeReference;
import java.math.BigDecimal;
import java.util.*;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class PermissionExpTemplateServiceImpl extends ServiceImpl<PermissionExpTemplateMapper, PermissionExpTemplate>
        implements
            PermissionExpTemplateService {

    private final PermissionExpService permissionExpService;

    /**
     * 创建权限表达式模版
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION_EXP_TEMPLATE, sysOperation = SysOperationType.CREATE, success = "创建了限制条件模板（{{ @linkGen.toLink(#templateId, T(ResourceType).PERMISSION_EXP_TEMPLATE) }}）", fail = "创建限制条件模板（{{ #requestDto.name }}）失败")
    @Transactional
    @Override
    public void createPermissionExpTemplate(PermissionExpTemplateRequestDto requestDto) {
        // 1. 检查模版参数
        checkParams(requestDto);

        // 2. 属性编辑
        String templateId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("templateId", templateId);

        PermissionExpTemplate permissionExpTemplate = new PermissionExpTemplate();
        permissionExpTemplate.setTemplateId(templateId);
        permissionExpTemplate.setTemplateName(requestDto.getName());
        permissionExpTemplate.setDescription(requestDto.getDesc());
        permissionExpTemplate.setExpression(requestDto.getExpression());
        if (CollectionUtils.isNotEmpty(requestDto.getParamConfigs())) {
            permissionExpTemplate.setTemplateParamConfigs(CommonUtil.serializeObject(requestDto.getParamConfigs()));
        }

        // 3. 数据库操作
        super.save(permissionExpTemplate);
    }

    /**
     * 更新权限表达式模版
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION_EXP_TEMPLATE, sysOperation = SysOperationType.UPDATE, success = "修改了限制条件模板（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).PERMISSION_EXP_TEMPLATE) }}）", fail = "修改限制条件模板（{{ #requestDto.name }}）失败")
    @Transactional
    @Override
    public void updatePermissionExpTemplate(PermissionExpTemplateRequestDto requestDto) {
        String templateId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 2. 获取版本号
        var rawPermissionExpTemplate = super.getById(requestDto.getId());
        if (Objects.isNull(rawPermissionExpTemplate)) {
            return;
        }
        compareObjBuilder.id(templateId);
        compareObjBuilder.before(rawPermissionExpTemplate);

        // 2. 检查模版参数
        checkParams(requestDto);

        // 3. 属性编辑
        PermissionExpTemplate updatePermissionExpTemplate = new PermissionExpTemplate();
        updatePermissionExpTemplate.setTemplateId(requestDto.getId());
        updatePermissionExpTemplate.setTemplateName(requestDto.getName());
        updatePermissionExpTemplate.setDescription(requestDto.getDesc());
        updatePermissionExpTemplate.setExpression(requestDto.getExpression());
        if (CollectionUtils.isNotEmpty(requestDto.getParamConfigs())) {
            updatePermissionExpTemplate
                    .setTemplateParamConfigs(CommonUtil.serializeObject(requestDto.getParamConfigs()));
        }

        // 3. 数据库操作
        super.updateById(updatePermissionExpTemplate);

        compareObjBuilder.after(super.getById(templateId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除权限表达式模版
     *
     * @param templateId
     *            模版ID
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION_EXP_TEMPLATE, sysOperation = SysOperationType.DELETE, success = "删除了限制条件模板（{{ @linkGen.toLink(#templateId, T(ResourceType).PERMISSION_EXP_TEMPLATE) }}）", fail = "删除限制条件模板（{{ @linkGen.toLink(#templateId, T(ResourceType).PERMISSION_EXP_TEMPLATE) }}）失败")
    @Transactional
    @Override
    public void deletePermissionExpTemplate(String templateId) {
        // 1. 数据库操作
        super.removeById(templateId);

        // 2. 删除关联的权限表达式
        permissionExpService.removeTemplatePermissionExp(templateId);
    }

    /**
     * 获取权限表达式模版参数配置
     *
     * @param templateId
     *            模版ID
     * @return 权限表达式模版参数配置
     */
    @Override
    public List<PermissionExpTemplateParamConfigDto> getParamsConfigs(String templateId) {
        // 1. 数据库操作
        PermissionExpTemplate permissionExpTemplate = super.getById(templateId);
        if (Objects.isNull(permissionExpTemplate)
                || StringUtils.isEmpty(permissionExpTemplate.getTemplateParamConfigs())) {
            return Collections.emptyList();
        }

        // 2. 反序列
        return CommonUtil.deserializeObject(permissionExpTemplate.getTemplateParamConfigs(),
                new TypeReference<List<PermissionExpTemplateParamConfigDto>>() {
                });
    }

    /**
     * 获取权限表达式模版列表
     *
     * @param page
     *            页数
     * @param size
     *            条数
     * @param keyword
     *            权限表达式模版名称检索关键字
     * @return 权限表达式模版列表
     */
    @Override
    public PageData<PermissionExpTemplateResponseDto> list(int page, int size, String keyword) {
        // 1. 数据库操作
        List<PermissionExpTemplate> permissionExpTemplateList;
        Page<PermissionExpTemplate> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            permissionExpTemplateList = super.list(pageRequest, Wrappers.<PermissionExpTemplate>lambdaQuery()
                    .like(PermissionExpTemplate::getTemplateName, keyword)
                    .orderByDesc(PermissionExpTemplate::getCreateTime));
        } else {
            permissionExpTemplateList = super.list(pageRequest, Wrappers.<PermissionExpTemplate>lambdaQuery()
                    .orderByDesc(PermissionExpTemplate::getCreateTime));
        }

        // 2. 属性设置
        var records = CommonUtil.stream(permissionExpTemplateList)
                .map(permissionExpTemplate -> PermissionExpTemplateResponseDto.builder()
                        .id(permissionExpTemplate.getTemplateId())
                        .name(permissionExpTemplate.getTemplateName())
                        .desc(permissionExpTemplate.getDescription())
                        .build())
                .toList();

        PageData<PermissionExpTemplateResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());
        pageData.setList(records);

        return pageData;
    }

    /**
     * 获取权限表达式模版详情
     *
     * @param templateId
     *            模版ID
     * @return 权限表达式模版详情
     */
    @Override
    public PermissionExpTemplateResponseDto detail(String templateId) {
        var responseBuilder = PermissionExpTemplateResponseDto.builder();

        // 1. 数据库操作
        PermissionExpTemplate permissionExpTemplate = super.getById(templateId);
        if (Objects.isNull(permissionExpTemplate)) {
            return responseBuilder.build();
        }

        // 2. 属性编辑
        responseBuilder.id(permissionExpTemplate.getTemplateId());
        responseBuilder.name(permissionExpTemplate.getTemplateName());
        responseBuilder.desc(permissionExpTemplate.getDescription());
        responseBuilder.expression(permissionExpTemplate.getExpression());
        if (StringUtils.isNotEmpty(permissionExpTemplate.getTemplateParamConfigs())) {
            responseBuilder.paramConfigs(CommonUtil.deserializeObject(permissionExpTemplate.getTemplateParamConfigs(),
                    new TypeReference<List<PermissionExpTemplateParamConfigDto>>() {
                    }));
        }
        return responseBuilder.build();
    }

    /**
     * 获取模板关联的权限表达式列表
     *
     * @param templateId
     *            模板ID
     * @return 关联的权限表达式列表
     */
    @Override
    public List<PermissionExpResponseDto> getPermissionExpList(String templateId) {
        // 1. 数据库操作
        List<PermissionExp> permissionExps = permissionExpService.list(Wrappers.<PermissionExp>lambdaQuery()
                .select(PermissionExp::getExpressionId, PermissionExp::getExpressionName, PermissionExp::getDescription)
                .eq(PermissionExp::getTemplateId, templateId)
                .orderByDesc(PermissionExp::getCreateTime));

        // 2. 属性编辑
        return CommonUtil.stream(permissionExps).map(permissionExp -> {
            PermissionExpResponseDto responseDto = new PermissionExpResponseDto();
            responseDto.setId(permissionExp.getExpressionId());
            responseDto.setName(permissionExp.getExpressionName());
            responseDto.setDesc(permissionExp.getDescription());
            return responseDto;
        }).toList();
    }

    /**
     * 获取模板参数对应的执行上下文
     *
     * @param paramConfigs
     *            模板参数配置
     * @param params
     *            参数列表
     * @return 执行上下文
     */
    @Override
    public Map<String, Object> getParamExecutionContext(List<PermissionExpTemplateParamConfigDto> paramConfigs,
            List<PermissionExpTemplateParamDto> params) {
        Map<String, Object> execCtx = new HashMap<>();
        for (var paramConfig : paramConfigs) {
            // 1. 获取匹配的参数
            var param = CommonUtil.stream(params)
                    .filter(templateParam -> templateParam.getCode().equals(paramConfig.getCode()))
                    .findFirst();
            if (param.isPresent()) {
                // 2. 转换参数类型
                execCtx.put(paramConfig.getCode(), convertParam(paramConfig.getType(), param.get().getValue()));
            } else {
                // 3. 无匹配参数，设置为null
                execCtx.put(paramConfig.getCode(), null);
            }
        }
        return execCtx;
    }

    private void checkParams(PermissionExpTemplateRequestDto requestDto) {
        List<PermissionExpTemplateParamConfigDto> paramConfigs = requestDto.getParamConfigs();
        if (CollectionUtils.isEmpty(paramConfigs)) {
            return;
        }

        // 1. 检查模版参数标识是否重复
        long distinctCnt = paramConfigs.stream().map(PermissionExpTemplateParamConfigDto::getCode).distinct().count();
        if (distinctCnt != paramConfigs.size()) {
            throw new BizException(MessageConstants.PERMISSION_EXP_TEMPLATE_MSG_1000);
        }

        // 2. 检查 CHOICE 类型参数
        for (PermissionExpTemplateParamConfigDto paramConfig : paramConfigs) {
            if (PermissionExpTemplateParamType.CHOICE.equals(paramConfig.getType())) {
                CommonUtil.validateBean(paramConfig, PermissionExpTemplateParamConfigDto.ChoiceType.class);
            } else {
                CommonUtil.validateBean(paramConfig);
            }
        }
    }

    private Object convertParam(PermissionExpTemplateParamType type, Object value) {
        if (Objects.isNull(value)) {
            return null;
        }
        return switch (type) {
            case NUMBER -> new BigDecimal(value.toString());
            case BOOLEAN -> Boolean.valueOf(value.toString());
            case LIST -> Arrays.asList(StringUtils.split(value.toString(), "\n"));
            case STRING, CHOICE -> value;
        };
    }
}
