package cn.opensrcdevelop.auth.biz.service.auth.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.component.LinkGenerator;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.PrincipalTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeConditionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRequestDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeCondition;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.mapper.auth.AuthorizeConditionMapper;
import cn.opensrcdevelop.auth.biz.mapper.auth.AuthorizeMapper;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeConditionService;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class AuthorizeServiceImpl extends ServiceImpl<AuthorizeMapper, AuthorizeRecord> implements AuthorizeService {

    private final AuthorizeConditionService authorizeConditionService;
    private final LinkGenerator linkGenerator;

    /**
     * 授权
     *
     * @param requestDto
     *            授权请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.CREATE, success = "给用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）、用户组（ "
            +
            "{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}）、角色（" +
            "{{ @linkGen.toLinks(#requestDto.roleIds, T(ResourceType).ROLE) }}" +
            "）授予了权限（{{ @linkGen.toLinks(#requestDto.permissionIds, T(ResourceType).PERMISSION) }}）", fail = "给用户（{{ @linkGen.toLinks(#requestDto.userIds, T(ResourceType).USER) }}）、用户组（ "
                    +
                    "{{ @linkGen.toLinks(#requestDto.userGroupIds, T(ResourceType).USER_GROUP) }}）、角色（" +
                    "{{ @linkGen.toLinks(#requestDto.roleIds, T(ResourceType).ROLE) }}" +
                    "）授予权限（{{ @linkGen.toLinks(#requestDto.permissionIds, T(ResourceType).PERMISSION) }}）失败")
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void authorize(AuthorizeRequestDto requestDto) {
        var userIds = requestDto.getUserIds();
        var userGroupIds = requestDto.getUserGroupIds();
        var roleIds = requestDto.getRoleIds();
        var permissionIds = requestDto.getPermissionIds();
        var expressionIds = requestDto.getExpressionIds();
        Integer priority = requestDto.getPriority();

        List<AuthorizeRecord> authorizeRecords = new ArrayList<>();
        var authorizeTime = LocalDateTime.now();
        // 1. 给用户授权
        if (CollectionUtils.isNotEmpty(userIds)) {
            userIds.forEach(userId -> permissionIds.forEach(permissionId -> {
                AuthorizeRecord authorizeRecord = new AuthorizeRecord();
                authorizeRecord.setAuthorizeId(CommonUtil.getUUIDV7String());
                authorizeRecord.setUserId(userId);
                authorizeRecord.setPermissionId(permissionId);
                authorizeRecord.setAuthorizeTime(authorizeTime);
                authorizeRecord.setPriority(priority);
                authorizeRecords.add(authorizeRecord);
            }));
        }

        // 2. 给用户组授权
        if (CollectionUtils.isNotEmpty(userGroupIds)) {
            userGroupIds.forEach(userGroupId -> permissionIds.forEach(permissionId -> {
                AuthorizeRecord authorizeRecord = new AuthorizeRecord();
                authorizeRecord.setAuthorizeId(CommonUtil.getUUIDV7String());
                authorizeRecord.setUserGroupId(userGroupId);
                authorizeRecord.setPermissionId(permissionId);
                authorizeRecord.setAuthorizeTime(authorizeTime);
                authorizeRecord.setPriority(priority);
                authorizeRecords.add(authorizeRecord);
            }));
        }

        // 3. 给角色授权
        if (CollectionUtils.isNotEmpty(roleIds)) {
            roleIds.forEach(roleId -> permissionIds.forEach(permissionId -> {
                AuthorizeRecord authorizeRecord = new AuthorizeRecord();
                authorizeRecord.setAuthorizeId(CommonUtil.getUUIDV7String());
                authorizeRecord.setRoleId(roleId);
                authorizeRecord.setPermissionId(permissionId);
                authorizeRecord.setAuthorizeTime(authorizeTime);
                authorizeRecord.setPriority(priority);
                authorizeRecords.add(authorizeRecord);
            }));
        }

        // 4. 删除待创建的授权，避免重复创建
        var targetPrincipalIds = CommonUtil.stream(authorizeRecords).map(authorizeRecord -> {
            if (Objects.nonNull(authorizeRecord.getUserId())) {
                return authorizeRecord.getUserId();
            }
            if (Objects.nonNull(authorizeRecord.getUserGroupId())) {
                return authorizeRecord.getUserGroupId();
            }
            if (Objects.nonNull(authorizeRecord.getRoleId())) {
                return authorizeRecord.getRoleId();
            }
            return null;
        }).filter(Objects::nonNull).toList();
        var targetPermissionIds = CommonUtil.stream(authorizeRecords).map(AuthorizeRecord::getPermissionId).toList();
        var query = Wrappers.<AuthorizeRecord>lambdaQuery().in(AuthorizeRecord::getPermissionId, targetPermissionIds)
                .and(o -> o.in(AuthorizeRecord::getUserId, targetPrincipalIds).or()
                        .in(AuthorizeRecord::getUserGroupId, targetPrincipalIds).or()
                        .in(AuthorizeRecord::getRoleId, targetPrincipalIds));
        var targetAuthorizeRecords = super.list(query);
        if (CollectionUtils.isNotEmpty(targetAuthorizeRecords)) {
            // 4.1 删除已存在的授权
            super.remove(query);

            // 4.2 删除关联的限制条件
            var targetAuthorizeIds = targetAuthorizeRecords.stream().map(AuthorizeRecord::getAuthorizeId).toList();
            authorizeConditionService.remove(Wrappers.<AuthorizeCondition>lambdaQuery()
                    .in(AuthorizeCondition::getAuthorizeId, targetAuthorizeIds));
        }

        // 5. 添加授权记录
        super.saveBatch(authorizeRecords);

        // 6. 添加限制条件
        if (CollectionUtils.isNotEmpty(expressionIds) && CollectionUtils.isNotEmpty(authorizeRecords)) {
            List<AuthorizeCondition> conditions = new ArrayList<>();
            expressionIds.forEach(expressionId -> authorizeRecords.forEach(authorizeRecord -> {
                AuthorizeCondition condition = new AuthorizeCondition();
                condition.setAuthorizeId(authorizeRecord.getAuthorizeId());
                condition.setPermissionExpId(expressionId);
                conditions.add(condition);
            }));
            authorizeConditionService.saveBatch(conditions);
        }
    }

    /**
     * 删除指定用户 / 用户组 / 角色的授权信息
     *
     * @param principalId
     *            用户 / 用户组 / 角色ID
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void removeAuthorization(String principalId) {
        var query = Wrappers.<AuthorizeRecord>lambdaQuery().eq(AuthorizeRecord::getUserId, principalId)
                .or(o -> o.eq(AuthorizeRecord::getUserGroupId, principalId))
                .or(o -> o.eq(AuthorizeRecord::getRoleId, principalId));
        // 1. 查询待删除的授权信息
        var records = super.list(query);

        if (CollectionUtils.isNotEmpty(records)) {
            // 2. 删除授权信息
            super.remove(query);

            // 3. 删除授权限定条件
            authorizeConditionService
                    .remove(Wrappers.<AuthorizeCondition>lambdaQuery().in(AuthorizeCondition::getAuthorizeId,
                            records.stream().map(AuthorizeRecord::getAuthorizeId).toList()));
        }
    }

    /**
     * 删除授权
     *
     * @param permissionId
     *            权限ID
     * @param principalId
     *            用户 / 用户组 / 角色ID
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.DELETE, success = "取消了对 {{ #principalPermission }}", fail = "取消对 {{ #principalPermission }} 失败")
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void removeAuthorization(String permissionId, String principalId) {
        var query = Wrappers.<AuthorizeRecord>lambdaQuery().eq(AuthorizeRecord::getPermissionId, permissionId)
                .and(o -> o.eq(AuthorizeRecord::getUserId, principalId)
                        .or(i -> i.eq(AuthorizeRecord::getUserGroupId, principalId))
                        .or(j -> j.eq(AuthorizeRecord::getRoleId, principalId)));
        // 1. 查询待删除的授权信息
        AuthorizeRecord authorizeRecord = super.getOne(query);

        if (authorizeRecord != null) {
            AuditContext.setSpelVariable("principalPermission", getAuditPrincipalPermission(authorizeRecord));

            // 2. 删除授权信息
            super.remove(query);

            // 3. 删除授权限定条件
            authorizeConditionService.remove(Wrappers.<AuthorizeCondition>lambdaQuery()
                    .eq(AuthorizeCondition::getAuthorizeId, authorizeRecord.getAuthorizeId()));
        }
    }

    /**
     * 删除授权
     *
     * @param permissionIds
     *            权限ID 集合
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void removeAuthorization(List<String> permissionIds) {
        var query = Wrappers.<AuthorizeRecord>lambdaQuery().in(AuthorizeRecord::getPermissionId, permissionIds);
        // 1. 查询待删除的授权信息
        var authorizeRecords = super.list(query);

        if (CollectionUtils.isNotEmpty(authorizeRecords)) {
            // 2. 删除授权信息
            super.remove(query);

            // 3. 删除授权条件
            var ids = authorizeRecords.stream().map(AuthorizeRecord::getAuthorizeId).toList();
            authorizeConditionService
                    .remove(Wrappers.<AuthorizeCondition>lambdaQuery().in(AuthorizeCondition::getAuthorizeId, ids));
        }
    }

    /**
     * 创建授权条件
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.CREATE, success = "给 {{ #principalPermissions }} 添加了限制条件（"
            +
            "{{ @linkGen.toLinks(#requestDto.permissionExpIds, T(ResourceType).PERMISSION_EXP) }}）", fail = "给 {{ #principalPermissions }} 添加限制条件（{{ @linkGen.toLinks(#requestDto.permissionExpIds, T(ResourceType).PERMISSION_EXP) }}）失败")
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void createAuthorizeCondition(AuthorizeConditionRequestDto requestDto) {

        // 1.待创建授权条件
        var conditions = getConditions(requestDto);

        if (CollectionUtils.isNotEmpty(conditions)) {
            AuditContext.setSpelVariable("principalPermissions",
                    getAuditPrincipalPermissions(requestDto.getAuthorizeIds()));

            // 2. 删除待创建的授权条件，避免重复创建
            SqlHelper.executeBatch(getSqlSessionFactory(), log, conditions, conditions.size(),
                    ((sqlSession, authorizeCondition) -> {
                        AuthorizeConditionMapper mapper = sqlSession.getMapper(AuthorizeConditionMapper.class);
                        mapper.delete(Wrappers.<AuthorizeCondition>lambdaQuery().eq(AuthorizeCondition::getAuthorizeId,
                                authorizeCondition.getAuthorizeId())
                                .and(o -> o.eq(AuthorizeCondition::getPermissionExpId,
                                        authorizeCondition.getPermissionExpId())));
                    }));

            // 3. 数据库操作
            authorizeConditionService.saveBatch(conditions);
        }
    }

    /**
     * 删除授权条件
     *
     * @param requestDto
     *            请求
     */
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Transactional
    @Override
    public void removeAuthorizeCondition(AuthorizeConditionRequestDto requestDto) {

        // 1. 待删除授权条件
        var targetConditions = getConditions(requestDto);

        if (CollectionUtils.isNotEmpty(targetConditions)) {
            // 2. 数据库操作
            SqlHelper.executeBatch(getSqlSessionFactory(), log, targetConditions, targetConditions.size(),
                    ((sqlSession, authorizeCondition) -> {
                        AuthorizeConditionMapper mapper = sqlSession.getMapper(AuthorizeConditionMapper.class);
                        mapper.delete(Wrappers.<AuthorizeCondition>lambdaQuery().eq(AuthorizeCondition::getAuthorizeId,
                                authorizeCondition.getAuthorizeId())
                                .and(o -> o.eq(AuthorizeCondition::getPermissionExpId,
                                        authorizeCondition.getPermissionExpId())));
                    }));
        }
    }

    /**
     * 更新授权优先级
     *
     * @param authorizeId
     *            授权ID
     * @param priority
     *            优先级
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.PERMISSION, sysOperation = SysOperationType.UPDATE, success = "将对 {{ #principalPermission }} 的优先级由 {{ #oldPriority }} 修改为了 {{ #newPriority }}", fail = "将对 {{ #principalPermission }} 的优先级由 {{ #oldPriority }} 修改为 {{ #newPriority }} 失败")
    @CacheEvict(cacheNames = CacheConstants.CACHE_CURRENT_USER_PERMISSIONS, allEntries = true)
    @Override
    public void updateAuthorizePriority(String authorizeId, Integer priority) {
        if (Objects.nonNull(priority)) {
            AuthorizeRecord authorizeRecord = super.getById(authorizeId);
            AuditContext.setSpelVariable("oldPriority", getAuthorizePriorityName(authorizeRecord.getPriority()));
            AuditContext.setSpelVariable("principalPermission", getAuditPrincipalPermission(authorizeRecord));
            AuditContext.setSpelVariable("newPriority", getAuthorizePriorityName(priority));

            // 1. 数据库操作
            super.update(Wrappers.<AuthorizeRecord>lambdaUpdate()
                    .set(AuthorizeRecord::getPriority, priority)
                    .eq(AuthorizeRecord::getAuthorizeId, authorizeId));
        }
    }

    private List<AuthorizeCondition> getConditions(AuthorizeConditionRequestDto requestDto) {
        var authorizeIds = CommonUtil.stream(requestDto.getAuthorizeIds()).filter(StringUtils::isNotBlank)
                .collect(Collectors.toSet());
        var permissionExpIds = CommonUtil.stream(requestDto.getPermissionExpIds()).filter(StringUtils::isNotBlank)
                .collect(Collectors.toSet());

        List<AuthorizeCondition> conditions = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(authorizeIds) && (CollectionUtils.isNotEmpty(permissionExpIds))) {
            authorizeIds.forEach(authorizeId -> permissionExpIds.forEach(permissionExpId -> {
                AuthorizeCondition condition = new AuthorizeCondition();
                condition.setAuthorizeId(authorizeId);
                condition.setPermissionExpId(permissionExpId);
                conditions.add(condition);
            }));
        }
        return conditions;
    }

    private String getAuditPrincipalPermission(AuthorizeRecord authorizeRecord) {
        String formatString = "%s（%s）的权限（%s）授权";
        if (Objects.nonNull(authorizeRecord.getUserId())) {
            return formatString.formatted(PrincipalTypeEnum.USER.getDisplayName(),
                    linkGenerator.toLink(authorizeRecord.getUserId(), ResourceType.USER),
                    linkGenerator.toLink(authorizeRecord.getPermissionId(), ResourceType.PERMISSION));
        }

        if (Objects.nonNull(authorizeRecord.getUserGroupId())) {
            return formatString.formatted(PrincipalTypeEnum.USER_GROUP.getDisplayName(),
                    linkGenerator.toLink(authorizeRecord.getUserGroupId(), ResourceType.USER_GROUP),
                    linkGenerator.toLink(authorizeRecord.getPermissionId(), ResourceType.PERMISSION));
        }

        if (Objects.nonNull(authorizeRecord.getRoleId())) {
            return formatString.formatted(PrincipalTypeEnum.ROLE.getDisplayName(),
                    linkGenerator.toLink(authorizeRecord.getRoleId(), ResourceType.ROLE),
                    linkGenerator.toLink(authorizeRecord.getPermissionId(), ResourceType.PERMISSION));
        }
        return StringUtils.EMPTY;
    }

    private String getAuditPrincipalPermissions(List<String> authorizeIds) {
        if (CollectionUtils.isNotEmpty(authorizeIds)) {
            var records = super.list(
                    Wrappers.<AuthorizeRecord>lambdaQuery().in(AuthorizeRecord::getAuthorizeId, authorizeIds));
            return "[ "
                    + CommonUtil.stream(records).map(this::getAuditPrincipalPermission).collect(Collectors.joining("，"))
                    + " ]";
        }

        return StringUtils.EMPTY;
    }

    private String getAuthorizePriorityName(Integer priority) {
        if (Objects.isNull(priority)) {
            return StringUtils.EMPTY;
        }

        return switch (priority) {
            case -1 -> "最低";
            case 0 -> "低";
            case 1 -> "中";
            case 2 -> "高";
            case 3 -> "最高";
            default -> StringUtils.EMPTY;
        };
    }
}
