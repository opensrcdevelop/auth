package cn.opensrcdevelop.auth.biz.service.permission.expression.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PrincipalTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.DebugPermissionExpRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.DebugPermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamConfigDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeCondition;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionExp;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionExpTemplate;
import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionExpMapper;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeConditionService;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpService;
import cn.opensrcdevelop.auth.biz.service.permission.expression.PermissionExpTemplateService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exression.ExpressionEngine;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.fasterxml.jackson.core.type.TypeReference;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.aop.framework.AopContext;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class PermissionExpServiceImpl extends ServiceImpl<PermissionExpMapper, PermissionExp> implements PermissionExpService {

    private final PermissionService permissionService;
    private final AuthorizeConditionService authorizeConditionService;
    private final ExpressionEngine expressionEngine;
    private final UserService userService;

    @Resource
    @Lazy
    private PermissionExpTemplateService permissionExpTemplateService;

    /**
     * 创建权限表达式
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.PERMISSION_EXP,
            sysOperation = SysOperationType.CREATE,
            success = "创建了限制条件（{{ @linkGen.toLink(#expressionId, T(ResourceType).PERMISSION_EXP) }}）",
            fail = "创建限制条件（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createPermissionExp(PermissionExpRequestDto requestDto) {
        // 1. 参数校验
        if (Boolean.TRUE.equals(requestDto.getUseTemplate())) {
            CommonUtil.validateBean(requestDto, PermissionExpRequestDto.UseTemplateInsert.class);
        } else {
            CommonUtil.validateBean(requestDto, PermissionExpRequestDto.NoneTemplateInsert.class);
        }

        // 2. 属性设置
        String expressionId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("expressionId", expressionId);

        PermissionExp permissionExp = new PermissionExp();
        permissionExp.setExpressionId(expressionId);
        permissionExp.setExpressionName(requestDto.getName());

        if (Boolean.TRUE.equals(requestDto.getUseTemplate())) {
            // 2.1 检查模板参数
            String templateId = requestDto.getTemplateId();
            var paramConfigs = permissionExpTemplateService.getParamsConfigs(templateId);
            var params = requestDto.getTemplateParams();
            checkParams(paramConfigs, params);

            permissionExp.setTemplateId(templateId);
            permissionExp.setTemplateParams(CommonUtil.serializeObject(requestDto.getTemplateParams()));
        } else {
            permissionExp.setExpression(requestDto.getExpression());
        }

        permissionExp.setDescription(requestDto.getDesc());

        // 3. 数据库操作
        super.save(permissionExp);
    }

    /**
     * 获取权限表达式列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 权限表达式名称检索关键字
     * @return 权限表达式列表
     */
    @Override
    public PageData<PermissionExpResponseDto> list(int page, int size, String keyword) {
        // 1. 数据库操作
        List<PermissionExp> permissionExps;
        Page<PermissionExp> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            permissionExps = super.list(pageRequest, Wrappers.<PermissionExp>lambdaQuery()
                    .select(PermissionExp::getExpressionId, PermissionExp::getExpressionName, PermissionExp::getDescription)
                    .like(PermissionExp::getExpressionName, keyword)
                    .orderByDesc(PermissionExp::getCreateTime));
        } else {
            permissionExps = super.list(pageRequest, Wrappers.<PermissionExp>lambdaQuery()
                    .select(PermissionExp::getExpressionId, PermissionExp::getExpressionName, PermissionExp::getDescription)
                    .orderByDesc(PermissionExp::getCreateTime));
        }

        // 2. 属性设置
        var records = CommonUtil.stream(permissionExps).map(permissionExp -> {
            PermissionExpResponseDto permissionExpResponse = new PermissionExpResponseDto();
            permissionExpResponse.setId(permissionExp.getExpressionId());
            permissionExpResponse.setName(permissionExp.getExpressionName());
            permissionExpResponse.setDesc(permissionExp.getDescription());

            return permissionExpResponse;
        }).toList();

        PageData<PermissionExpResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());
        pageData.setList(records);
        return pageData;
    }

    /**
     * 获取权限表达式详情
     *
     * @param permissionExpId 权限表达式ID
     * @return 权限表达式详情
     */
    @Override
    public PermissionExpResponseDto detail(String permissionExpId) {
        PermissionExpResponseDto permissionExpResponse = new PermissionExpResponseDto();
        // 1. 数据库操作
        PermissionExp permissionExp = super.getById(permissionExpId);

        if (Objects.isNull(permissionExp)) {
            return permissionExpResponse;
        }

        // 2. 属性编辑
        permissionExpResponse.setId(permissionExp.getExpressionId());
        permissionExpResponse.setName(permissionExp.getExpressionName());
        permissionExpResponse.setUseTemplate(Objects.nonNull(permissionExp.getTemplateId()));
        permissionExpResponse.setExpression(permissionExp.getExpression());
        permissionExpResponse.setTemplateId(permissionExp.getTemplateId());
        if (Objects.nonNull(permissionExp.getTemplateParams())) {
            permissionExpResponse.setTemplateParams(CommonUtil.deserializeObject(permissionExp.getTemplateParams(), new TypeReference<List<PermissionExpTemplateParamDto>>() {}));
        }
        permissionExpResponse.setDesc(permissionExp.getDescription());
        return permissionExpResponse;
    }

    /**
     * 更新权限表达式
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.PERMISSION_EXP,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了限制条件（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).PERMISSION_EXP) }}）",
            fail = "修改限制条件（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).PERMISSION_EXP) }}）失败"
    )
    @Caching(
            evict = {
                    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true),
                    @CacheEvict(cacheNames = CacheConstants.CACHE_PERMISSION_EXP, key = "#root.target.generatePermissionExpCacheKey(#permissionExpId)")
            }
    )
    @Transactional
    @Override
    public void updatePermissionExp(PermissionExpRequestDto requestDto) {
        String expressionId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawPermissionExp = super.getById(requestDto.getId());
        if (Objects.isNull(rawPermissionExp)) {
            return;
        }
        compareObjBuilder.id(expressionId);
        compareObjBuilder.before(rawPermissionExp);

        // 2. 参数校验
        if (Boolean.TRUE.equals(requestDto.getUseTemplate())) {
            CommonUtil.validateBean(requestDto, PermissionExpRequestDto.UseTemplateUpdate.class);
        } else {
            CommonUtil.validateBean(requestDto, PermissionExpRequestDto.NoneTemplateUpdate.class);
        }

        // 3. 属性设置
        PermissionExp permissionExp = new PermissionExp();
        permissionExp.setExpressionId(expressionId);
        permissionExp.setExpressionName(requestDto.getName());
        if (Boolean.TRUE.equals(requestDto.getUseTemplate())) {
            permissionExp.setTemplateParams(CommonUtil.serializeObject(requestDto.getTemplateParams()));
        } else {
            permissionExp.setExpression(requestDto.getExpression());
        }
        permissionExp.setDescription(requestDto.getDesc());
        permissionExp.setVersion(rawPermissionExp.getVersion());

        // 4. 数据库操作
        super.updateById(permissionExp);

        compareObjBuilder.after(super.getById(expressionId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除权限表达式
     *
     * @param permissionExpId 权限表达式ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.PERMISSION_EXP,
            sysOperation = SysOperationType.DELETE,
            success = "删除了限制条件（{{ @linkGen.toLink(#permissionExpId, T(ResourceType).PERMISSION_EXP) }}）",
            fail = "删除限制条件（{{ @linkGen.toLink(#permissionExpId, T(ResourceType).PERMISSION_EXP) }}）失败"
    )
    @Caching(
            evict = {
                    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true),
                    @CacheEvict(cacheNames = CacheConstants.CACHE_PERMISSION_EXP, key = "#root.target.generatePermissionExpCacheKey(#permissionExpId)")
            }
    )
    @Transactional
    @Override
    public void removePermissionExp(String permissionExpId) {
        // 1. 删除权限表达式
        super.removeById(permissionExpId);

        // 2. 删除关联的全部的授权记录
        authorizeConditionService.remove(Wrappers.<AuthorizeCondition>lambdaQuery().eq(AuthorizeCondition::getPermissionExpId, permissionExpId));
    }

    /**
     * 调试权限表达式
     *
     * @param requestDto 请求
     * @return 响应
     */
    @Override
    public DebugPermissionExpResponseDto debugPermissionExp(DebugPermissionExpRequestDto requestDto) {
        // 1. 参数校验
        boolean useTemplate = Boolean.TRUE.equals(requestDto.getUseTemplate());
        if (useTemplate) {
            CommonUtil.validateBean(requestDto, DebugPermissionExpRequestDto.UseTemplate.class);
        } else {
            CommonUtil.validateBean(requestDto, DebugPermissionExpRequestDto.NotUseTemplate.class);
        }

        // 2. 获取表达式 & 添加执行上下文
        Map<String, Object> execCtx = new HashMap<>();
        String expression;
        if (useTemplate) {
            PermissionExpTemplate permissionExpTemplate = permissionExpTemplateService.getById(requestDto.getTemplateId());
            if (Objects.isNull(permissionExpTemplate)) {
                throw new BizException(MessageConstants.PERMISSION_EXP_MSG_1003, requestDto.getTemplateId());
            }
            if (StringUtils.isNotEmpty(permissionExpTemplate.getTemplateParamConfigs())) {
                var paramConfigs = CommonUtil.deserializeObject(permissionExpTemplate.getTemplateParamConfigs(), new TypeReference<List<PermissionExpTemplateParamConfigDto>>() {});
                // 2.1 校验模板参数
                checkParams(paramConfigs, requestDto.getTemplateParams());

                // 2.2 添加模板参数执行上下文
                execCtx.putAll(permissionExpTemplateService.getParamExecutionContext(paramConfigs, requestDto.getTemplateParams()));
            }
            expression = permissionExpTemplate.getExpression();
        } else {
            PermissionExp permissionExp = super.getById(requestDto.getExpressionId());
            if (Objects.isNull(permissionExp)) {
                throw new BizException(MessageConstants.PERMISSION_EXP_MSG_1000, requestDto.getExpressionId());
            }
            expression = permissionExp.getExpression();
        }

        // 2. 执行表达式
        DebugPermissionExpResponseDto responseDto = new DebugPermissionExpResponseDto();
        // 2.2 添加上下文
        if (MapUtils.isNotEmpty(requestDto.getContext())) {
            execCtx.putAll(requestDto.getContext());
        }

        Object execResult = null;
        try {
            // 2.3 执行
            execResult = expressionEngine.evaluate(expression, execCtx);
        } catch (Exception e) {
            responseDto.setSuccess(Boolean.FALSE);
            responseDto.setExecuteRes(e.getMessage());
            return responseDto;
        }

        if (!(execResult instanceof Boolean)) {
            throw new BizException(MessageConstants.PERMISSION_EXP_MSG_1004);
        }
        responseDto.setSuccess(Boolean.TRUE);
        responseDto.setExecuteRes(execResult);
        return responseDto;
    }

    /**
     * 获取权限表达式关联的权限列表
     *
     * @param permissionExpId 权限表达式ID
     * @return 权限表达式关联的权限列表
     */
    @Override
    public List<PermissionResponseDto> expPermissions(String permissionExpId) {
        // 1. 数据库操作
        List<AuthorizeRecord> authorizeRecords = permissionService.getExpPermissions(permissionExpId);

        // 2. 属性编辑
        return CommonUtil.stream(authorizeRecords).map(authorizeRecord -> {
            PermissionResponseDto permissionResponse = new PermissionResponseDto();
            permissionResponse.setAuthorizeId(authorizeRecord.getAuthorizeId());

            var permission = authorizeRecord.getPermission();
            permissionResponse.setPermissionId(permission.getPermissionId());
            permissionResponse.setPermissionName(permission.getPermissionName());
            permissionResponse.setPermissionCode(permission.getPermissionCode());
            permissionResponse.setResourceId(permission.getResource().getResourceId());
            permissionResponse.setResourceCode(permission.getResource().getResourceCode());
            permissionResponse.setResourceName(permission.getResource().getResourceName());
            permissionResponse.setResourceGroupId(permission.getResource().getResourceGroup().getResourceGroupId());
            permissionResponse.setResourceGroupCode(permission.getResource().getResourceGroup().getResourceGroupCode());
            permissionResponse.setResourceGroupName(permission.getResource().getResourceGroup().getResourceGroupName());

            // 2.1 被授权主体和主体类型
            var permissionPrincipal = getPermissionPrincipal(authorizeRecord);
            permissionResponse.setPrincipalId(permissionPrincipal._1);
            permissionResponse.setPrincipal(permissionPrincipal._2);
            permissionResponse.setPrincipalType(permissionPrincipal._3);
            permissionResponse.setPrincipalTypeDisplayName(permissionPrincipal._4);
            return permissionResponse;
        }).toList();
    }

    /**
     * 获取权限表达式（使用缓存）
     *
     * @param permissionExpId 权限表达式ID
     * @return 权限表达式
     */
    @Cacheable(
            cacheNames = CacheConstants.CACHE_PERMISSION_EXP,
            key = "#root.target.generatePermissionExpCacheKey(#permissionExpId)",
            condition = "#result != null"
    )
    @Override
    public PermissionExp getPermissionExpWithCache(String permissionExpId) {
        PermissionExp permissionExp = super.getById(permissionExpId);
        if (Objects.nonNull(permissionExp.getTemplateId())) {
            permissionExp.setPermissionExpTemplate(permissionExpTemplateService.getById(permissionExp.getTemplateId()));
        }
        return permissionExp;
    }

    /**
     * 执行权限表达式
     *
     * @param permissionExpId 权限表达式ID
     * @param customCtx      自定义上下文
     * @return 执行结果
     */
    @Override
    public Boolean executePermissionExp(String permissionExpId, Map<String, Object> customCtx) {
        // 1. 获取权限表达式
        PermissionExpService proxyService = (PermissionExpService) AopContext.currentProxy();
        PermissionExp permissionExp = proxyService.getPermissionExpWithCache(permissionExpId);

        // 2. 构建执行上下文
        Map<String, Object> executeCtx = new HashMap<>();

        // 2.1 添加请求环境属性上下文
        Map<String, Object> reqCtx = new HashMap<>();
        reqCtx.put(CommonConstants.REQ_CTX_IP, WebUtil.getRemoteIP());
        reqCtx.put(CommonConstants.REQ_CTX_BROWSER_TYPE, WebUtil.getBrowserTypeNoVersion());
        reqCtx.put(CommonConstants.REQ_CTX_DEVICE_TYPE, WebUtil.getDeviceType());
        reqCtx.put(CommonConstants.REQ_CTX_OS_TYPE, WebUtil.getDeviceOsNoVersion());
        executeCtx.put(CommonConstants.REQ_CTX, reqCtx);

        // 2.2 添加请求用户属性上下文
        executeCtx.put(CommonConstants.USER_CTX, userService.getCurrentUserInfo());

        if (Objects.nonNull(permissionExp.getTemplateId())) {
            // 2.3 添加模板参数上下文
            var paramConfigs = CommonUtil.deserializeObject(permissionExp.getPermissionExpTemplate().getTemplateParamConfigs(),
                    new TypeReference<List<PermissionExpTemplateParamConfigDto>>() {});
            var params= CommonUtil.deserializeObject(permissionExp.getTemplateParams(),
                    new TypeReference<List<PermissionExpTemplateParamDto>>() {});
            executeCtx.putAll(permissionExpTemplateService.getParamExecutionContext(paramConfigs, params));
        }

        // 2.4 添加自定义上下文
        if (MapUtils.isNotEmpty(customCtx)) {
            executeCtx.putAll(customCtx);
        }

        // 3. 执行表达式
        String expression = Objects.nonNull(permissionExp.getTemplateId()) ? permissionExp.getPermissionExpTemplate().getExpression() : permissionExp.getExpression();
        Object result = expressionEngine.evaluate(expression, executeCtx);
        if (!(result instanceof Boolean)) {
            return false;
        }
        return (Boolean) result;
    }

    /**
     * 删除模板关联的权限表达式
     *
     * @param templateId 模板ID
     */
    @Caching(
            evict = {
                    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true),
                    @CacheEvict(cacheNames = CacheConstants.CACHE_PERMISSION_EXP, allEntries = true)
            }
    )
    @Transactional
    @Override
    public void removeTemplatePermissionExp(String templateId) {
        // 1. 数据库操作
        super.remove(Wrappers.<PermissionExp>lambdaQuery().eq(PermissionExp::getTemplateId, templateId));
    }

    /**
     * 生成 Redis 缓存 key
     *
     * @return Redis 缓存 key
     */
    public String generatePermissionExpCacheKey(String permissionExpId) {
        return TenantContextHolder.getTenantContext().getTenantCode() + ":" + permissionExpId;
    }

    private Tuple4<String, String, String, String> getPermissionPrincipal(AuthorizeRecord authorizeRecord) {
        User user = authorizeRecord.getUser();
        UserGroup userGroup = authorizeRecord.getUserGroup();
        Role role = authorizeRecord.getRole();

        if (user != null) {
            return Tuple.of(user.getUserId(), user.getUsername(), PrincipalTypeEnum.USER.getType(), PrincipalTypeEnum.USER.getDisplayName());
        }

        if (userGroup != null) {
            return Tuple.of(userGroup.getUserGroupId(), userGroup.getUserGroupName(), PrincipalTypeEnum.USER_GROUP.getType(), PrincipalTypeEnum.USER_GROUP.getDisplayName());
        }

        if (role != null) {
            return Tuple.of(role.getRoleId(), role.getRoleName(), PrincipalTypeEnum.ROLE.getType(), PrincipalTypeEnum.ROLE.getDisplayName());
        }
        return Tuple.of(null, null, null, null);
    }

    private void checkParams(List<PermissionExpTemplateParamConfigDto> paramConfigs, List<PermissionExpTemplateParamDto> params) {
        if (CollectionUtils.isEmpty(paramConfigs)) {
            return;
        }

        // 1. 检查模板参数是否填写完整
        if (CollectionUtils.isEmpty(params) || params.size() != paramConfigs.size()) {
            throw new BizException(MessageConstants.PERMISSION_EXP_MSG_1001);
        }

        // 2 检查必填参数是否填写
        var requiredParamCodes = paramConfigs.stream().filter(PermissionExpTemplateParamConfigDto::getRequired)
                .map(PermissionExpTemplateParamConfigDto::getCode).toList();
        var paramCodes = params.stream().map(PermissionExpTemplateParamDto::getCode).toList();
        var notInputtedParamCodes = CommonUtil.stream(requiredParamCodes).filter(paramCode -> !paramCodes.contains(paramCode)).toList();
        if (CollectionUtils.isNotEmpty(notInputtedParamCodes)) {
            throw new BizException(MessageConstants.PERMISSION_EXP_MSG_1002, notInputtedParamCodes);
        }
    }
}
