package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PrincipalTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.permission.*;
import cn.opensrcdevelop.auth.biz.entity.*;
import cn.opensrcdevelop.auth.biz.mapper.PermissionExpressionMapper;
import cn.opensrcdevelop.auth.biz.service.AuthorizeConditionService;
import cn.opensrcdevelop.auth.biz.service.PermissionExpService;
import cn.opensrcdevelop.auth.biz.service.PermissionService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class PermissionExpServiceImpl extends ServiceImpl<PermissionExpressionMapper, PermissionExp> implements PermissionExpService {

    private final PermissionService permissionService;
    private final AuthorizeConditionService authorizeConditionService;

    /**
     * 创建权限表达式
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createPermissionExp(PermissionExpRequestDto requestDto) {
        // 1. 属性设置
        PermissionExp permissionExp = new PermissionExp();
        permissionExp.setExpressionId(CommonUtil.getUUIDString());
        permissionExp.setExpressionName(requestDto.getName());
        permissionExp.setExpression(requestDto.getExpression());
        permissionExp.setDescription(requestDto.getDesc());

        // 2. 数据库操作
        super.save(permissionExp);
    }

    /**
     * 获取权限表达式列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 表达式 / 名称检索关键字
     * @return 权限表达式列表
     */
    @Override
    public PageData<PermissionExpResponseDto> list(int page, int size, String keyword) {
        // 1. 数据库操作
        List<PermissionExp> permissionExps;
        Page<PermissionExp> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            permissionExps = super.list(pageRequest, Wrappers.<PermissionExp>lambdaQuery()
                    .like(PermissionExp::getExpressionName, keyword)
                    .or(o -> o.like(PermissionExp::getExpression, keyword))
                    .orderByAsc(PermissionExp::getExpressionName));
        } else {
            permissionExps = super.list(pageRequest, Wrappers.<PermissionExp>lambdaQuery().orderByAsc(PermissionExp::getExpressionName));
        }

        // 2. 属性设置
        var records = CommonUtil.stream(permissionExps).map(permissionExp -> {
            PermissionExpResponseDto permissionExpResponse = new PermissionExpResponseDto();
            permissionExpResponse.setId(permissionExp.getExpressionId());
            permissionExpResponse.setName(permissionExp.getExpressionName());
            permissionExpResponse.setExpression(permissionExp.getExpression());

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
     * @param expressionId 表达式ID
     * @return 权限表达式详情
     */
    @Override
    public PermissionExpResponseDto detail(String expressionId) {
        PermissionExpResponseDto permissionExpResponse = new PermissionExpResponseDto();
        // 1. 基本信息
        PermissionExp permissionExp = super.getById(expressionId);
        permissionExpResponse.setId(permissionExp.getExpressionId());
        permissionExpResponse.setName(permissionExp.getExpressionName());
        permissionExpResponse.setExpression(permissionExp.getExpression());
        permissionExpResponse.setDesc(permissionExp.getDescription());

        // 2. 权限信息
        var permissions = CommonUtil.stream(permissionService.getExpPermissions(expressionId)).map(authorizeRecord -> {
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
        permissionExpResponse.setPermissions(permissions);

        return permissionExpResponse;
    }

    /**
     * 更新权限表达式
     *
     * @param requestDto 请求
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void updatePermissionExp(PermissionExpRequestDto requestDto) {
        // 1. 获取版本号
        var rawPermissionExp = super.getById(requestDto.getId());
        if (Objects.isNull(rawPermissionExp)) {
            return;
        }

        // 2. 属性设置
        PermissionExp permissionExp = new PermissionExp();
        permissionExp.setExpressionId(requestDto.getId());
        permissionExp.setExpressionName(requestDto.getName());
        permissionExp.setExpression(requestDto.getExpression());
        permissionExp.setDescription(requestDto.getDesc());
        permissionExp.setVersion(rawPermissionExp.getVersion());

        // 3. 数据库操作
        super.updateById(permissionExp);
    }

    /**
     * 删除权限表达式
     *
     * @param permissionExpId 权限表达式ID
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
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
        // 1. 获取权限表达式
        PermissionExp permissionExp = super.getById(requestDto.getExpressionId());
        if (Objects.isNull(permissionExp)) {
            throw new BizException(MessageConstants.PERMISSION_EXP_MSG_1000, requestDto.getExpressionId());
        }
        String expression = permissionExp.getExpression();

        // 2. 执行表达式
        DebugPermissionExpResponseDto responseDto = new DebugPermissionExpResponseDto();
        try {
            SpelExpressionParser spelExpressionParser = new SpelExpressionParser();
            StandardEvaluationContext standardEvaluationContext = new StandardEvaluationContext(requestDto.getContext());
            standardEvaluationContext.addPropertyAccessor(new MapAccessor());
            Boolean res = spelExpressionParser.parseExpression(expression).getValue(standardEvaluationContext, Boolean.class);
            responseDto.setIsSuccess(Boolean.TRUE);
            responseDto.setExecuteRes(res);
        } catch (Exception e) {
            responseDto.setIsSuccess(Boolean.FALSE);
            responseDto.setExecuteRes(e.getMessage());
        }
        return responseDto;
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
}
