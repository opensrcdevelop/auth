package cn.opensrcdevelop.auth.biz.service.system.password.impl;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.system.password.*;
import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicy;
import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicyMapping;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mapper.system.password.PasswordPolicyMapper;
import cn.opensrcdevelop.auth.biz.repository.system.password.PasswordPolicyRepository;
import cn.opensrcdevelop.auth.biz.service.system.password.PasswordPolicyMappingService;
import cn.opensrcdevelop.auth.biz.service.system.password.PasswordPolicyService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.security.password.PasswordComplexityConfig;
import cn.opensrcdevelop.common.security.password.PasswordStrength;
import cn.opensrcdevelop.common.security.password.strength.*;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class PasswordPolicyServiceImpl extends ServiceImpl<PasswordPolicyMapper, PasswordPolicy> implements PasswordPolicyService {

    private static final String DEFAULT_POLICY_NAME = "默认策略";

    private final PasswordPolicyMappingService passwordPolicyMappingService;
    private final PasswordPolicyRepository passwordPolicyRepository;

    @Resource
    @Lazy
    private UserService userService;

    /**
     *  获取密码策略列表
     *
     * @return 密码策略列表
     */
    @Override
    public List<PasswordPolicyResponseDto> getList() {
        // 1. 数据库查询
        // 1.1 按照优先级排序获取密码策略列表
        List<PasswordPolicy> records = super.list(Wrappers.<PasswordPolicy>lambdaQuery().orderByAsc(PasswordPolicy::getPriority));

        // 2. 响应编辑
        return CommonUtil.stream(records).map(passwordPolicy ->
                PasswordPolicyResponseDto.builder()
                    .id(passwordPolicy.getPolicyId())
                    .name(passwordPolicy.getPolicyName())
                    .passwordStrength(passwordPolicy.getPasswordStrength())
                    .enabled(passwordPolicy.getEnabled())
                    .build())
                .toList();
    }

    /**
     * 创建密码策略请求
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createPasswordPolicy(PasswordPolicyRequestDto requestDto) {
        // 1. 参数校验
        checkRequestDto(requestDto);

        // 2. 属性编辑
        String policyId = CommonUtil.getUUIDString();
        PasswordPolicy passwordPolicy = new PasswordPolicy();
        passwordPolicy.setPolicyId(policyId);
        passwordPolicy.setPolicyName(requestDto.getName());
        passwordPolicy.setDescription(requestDto.getDesc());
        passwordPolicy.setEnabled(Boolean.TRUE);
        passwordPolicy.setPriority(0);
        passwordPolicy.setPasswordStrength(requestDto.getPasswordStrength());
        passwordPolicy.setEnablePasswordDetection(requestDto.getEnablePasswordDetection());
        passwordPolicy.setEnableForceChangePassword(requestDto.getEnableForceChangePassword());
        passwordPolicy.setForcedCycle(requestDto.getForcedCycle());
        passwordPolicy.setForcedCycleUnit(requestDto.getForcedCycleUnit());
        passwordPolicy.setRemindCycle(requestDto.getRemindCycle());
        passwordPolicy.setRemindCycleUnit(requestDto.getRemindCycleUnit());

        // 2.1 自定义密码强度配置
        if (requestDto.getPasswordStrength().equals(PasswordStrength.CUSTOM.ordinal())) {
            passwordPolicy.setCustomStrengthConfig(CommonUtil.serializeObject(buildComplexityConfig(requestDto)));
        }

        // 3. 保存
        // 3.1 将其他密码策略优先级 + 1
        super.update(Wrappers.<PasswordPolicy>lambdaUpdate().setSql("priority = priority + 1"));
        // 3.2 保存本次新增的密码策略
        super.save(passwordPolicy);

        // 4. 保存策略应用主体
        List<PasswordPolicyMapping> mappings = getMappings(requestDto, policyId);
        passwordPolicyMappingService.saveBatch(mappings);
    }

    /**
     *  获取密码策略详情
     *
     * @param id 密码策略 ID
     * @return 密码策略详情
     */
    @Override
    public PasswordPolicyResponseDto detail(String id) {
        // 1. 数据库查询
        PasswordPolicy passwordPolicy = super.getById(id);
        if (Objects.isNull(passwordPolicy)) {
            return PasswordPolicyResponseDto.builder().build();
        }

        // 1.1 提取密码强度配置
        PasswordComplexityConfig complexityConfig = null;
        if (passwordPolicy.getPasswordStrength().equals(PasswordStrength.CUSTOM.ordinal())) {
            complexityConfig = CommonUtil.deserializeObject(passwordPolicy.getCustomStrengthConfig(), PasswordComplexityConfig.class);
        }

        // 2. 查询策略应用主体
        List<PasswordPolicyMapping> mappings = passwordPolicyMappingService.list(Wrappers.<PasswordPolicyMapping>lambdaQuery().eq(PasswordPolicyMapping::getPolicyId, passwordPolicy.getPolicyId()));
        // 2.1 提取用户 ID 和用户组 ID
        List<String> userIds = CommonUtil.stream(mappings).map(PasswordPolicyMapping::getUserId).filter(Objects::nonNull).toList();
        List<String> userGroupIds = CommonUtil.stream(mappings).map(PasswordPolicyMapping::getUserGroupId).filter(Objects::nonNull).toList();

        // 3. 响应编辑
        PasswordPolicyResponseDto responseDto = PasswordPolicyResponseDto.builder()
                .id(passwordPolicy.getPolicyId())
                .name(passwordPolicy.getPolicyName())
                .desc(passwordPolicy.getDescription())
                .passwordStrength(passwordPolicy.getPasswordStrength())
                .enablePasswordDetection(passwordPolicy.getEnablePasswordDetection())
                .enableForceChangePassword(passwordPolicy.getEnableForceChangePassword())
                .forcedCycle(passwordPolicy.getForcedCycle())
                .forcedCycleUnit(passwordPolicy.getForcedCycleUnit())
                .remindCycle(passwordPolicy.getRemindCycle())
                .remindCycleUnit(passwordPolicy.getRemindCycleUnit()).build();

        // 3.1 策略应用主体
        if (CollectionUtils.isNotEmpty(userIds)) {
            responseDto.setUserIds(userIds);
        }
        if (CollectionUtils.isNotEmpty(userGroupIds)) {
            responseDto.setUserGroupIds(userGroupIds);
        }

        // 3.2 自定义密码强度配置
        if (Objects.nonNull(complexityConfig)) {
            responseDto.setMinLength(complexityConfig.getMinLength());
            responseDto.setMaxLength(complexityConfig.getMaxLength());
            responseDto.setRequireNumber(complexityConfig.isRequireNumber());
            responseDto.setRequireUpperCase(complexityConfig.isRequireUpperCase());
            responseDto.setRequireLowerCase(complexityConfig.isRequireLowerCase());
            responseDto.setRequireSpecialChar(complexityConfig.isRequireSpecialChar());
            responseDto.setMinCharTypeCount(complexityConfig.getMinCharTypeCount());
            responseDto.setProhibitSingleChar(complexityConfig.isProhibitSingleChar());
            responseDto.setProhibitConsecutiveChar(complexityConfig.isProhibitConsecutiveChar());
            responseDto.setProhibitContainConsecutiveChar(complexityConfig.isProhibitContainConsecutiveChar());
            responseDto.setMinConsecutiveCharLength(complexityConfig.getMinConsecutiveCharLength());
            responseDto.setProhibitContainRepeatChar(complexityConfig.isProhibitContainRepeatChar());
            responseDto.setMinRepeatCharLength(complexityConfig.getMinRepeatCharLength());
            responseDto.setProhibitUserInfo(complexityConfig.isProhibitUserInfo());
            responseDto.setProhibitSpecificPassword(complexityConfig.isProhibitSpecificPassword());
            if (CollectionUtils.isNotEmpty(complexityConfig.getProhibitedPasswordList())) {
                responseDto.setProhibitedPasswordList(complexityConfig.getProhibitedPasswordList());
            }
        }
        return responseDto;
    }

    /**
     * 更新密码策略优先级请求
     *
     * @param requestDtoList 请求
     */
    @Transactional
    @Override
    public void updatePasswordPolicyPriority(List<UpdatePasswordPolicyPriorityRequestDto> requestDtoList) {
        // 1. 检查最小优先级密码策略 ID 是否与请求中的最后一个 ID 一致
        if (!StringUtils.equals(getDefaultPasswordPolicy().getPolicyId(), requestDtoList.getLast().getId())) {
            throw new BizException(MessageConstants.PWD_POLICY_MSG_1006);
        }

        // 2. 更新密码策略优先级
        List<PasswordPolicy> updateRecords = CommonUtil.stream(requestDtoList)
                .map(req -> {
                    PasswordPolicy passwordPolicy = new PasswordPolicy();
                    passwordPolicy.setPolicyId(req.getId());
                    passwordPolicy.setPriority(req.getPriority());

                    return passwordPolicy;
                })
                .toList();
        super.updateBatchById(updateRecords);
    }

    /**
     * 更新密码策略
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updatePasswordPolicy(PasswordPolicyRequestDto requestDto) {
        PasswordPolicy defaultPasswordPolicy = getDefaultPasswordPolicy();
        PasswordPolicy rawPasswordPolicy = super.getById(requestDto.getId());
        // 1. 判断是否更新状态
        if (Objects.nonNull(requestDto.getEnabled())) {
            // 1.1 判断是否为默认密码策略
            if (requestDto.getId().equals(defaultPasswordPolicy.getPolicyId())) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1007);
            }

            PasswordPolicy updatePasswordPolicy = new PasswordPolicy();
            updatePasswordPolicy.setPolicyId(requestDto.getId());
            updatePasswordPolicy.setEnabled(requestDto.getEnabled());
            updatePasswordPolicy.setVersion(rawPasswordPolicy.getVersion());

            // 1.2 更新状态
            super.updateById(updatePasswordPolicy);
            return;
        }

        // 2. 参数校验
        checkRequestDto(requestDto);
        // 2.1 禁止更新默认密码策略名称
        if (requestDto.getId().equals(defaultPasswordPolicy.getPolicyId()) && !requestDto.getName().equals(getDefaultPasswordPolicy().getPolicyName())) {
            throw new BizException(MessageConstants.PWD_POLICY_MSG_1007);
        }

        // 3. 更新策略应用主体
        List<PasswordPolicyMapping> mappings = getMappings(requestDto, requestDto.getId());
        if (CollectionUtils.isNotEmpty(mappings)) {
            // 3.1 删除原有的策略应用主体
            passwordPolicyMappingService.remove(Wrappers.<PasswordPolicyMapping>lambdaQuery().eq(PasswordPolicyMapping::getPolicyId, requestDto.getId()));
            // 3.2 保存策略应用主体
            passwordPolicyMappingService.saveBatch(mappings);
        }

        // 4. 更新密码策略
        // 4.1 属性编辑
        PasswordPolicy updatePasswordPolicy = new PasswordPolicy();
        updatePasswordPolicy.setPolicyId(requestDto.getId());
        updatePasswordPolicy.setPolicyName(requestDto.getName());
        updatePasswordPolicy.setDescription(requestDto.getDesc());
        updatePasswordPolicy.setEnabled(requestDto.getEnabled());
        updatePasswordPolicy.setPasswordStrength(requestDto.getPasswordStrength());
        updatePasswordPolicy.setEnablePasswordDetection(requestDto.getEnablePasswordDetection());
        updatePasswordPolicy.setEnableForceChangePassword(requestDto.getEnableForceChangePassword());
        updatePasswordPolicy.setForcedCycle(requestDto.getForcedCycle());
        updatePasswordPolicy.setForcedCycleUnit(requestDto.getForcedCycleUnit());
        updatePasswordPolicy.setRemindCycle(requestDto.getRemindCycle());
        updatePasswordPolicy.setRemindCycleUnit(requestDto.getRemindCycleUnit());
        updatePasswordPolicy.setVersion(rawPasswordPolicy.getVersion());

        // 4.2 自定义密码强度配置
        if (requestDto.getPasswordStrength().equals(PasswordStrength.CUSTOM.ordinal())) {
            updatePasswordPolicy.setCustomStrengthConfig(CommonUtil.serializeObject(buildComplexityConfig(requestDto)));
        }
        // 4.3 数据库操作
        super.updateById(updatePasswordPolicy);
    }

    /**
     * 删除密码策略
     *
     * @param id 密码策略 ID
     */
    @Transactional
    @Override
    public void deletePasswordPolicy(String id) {
        // 1. 判断是否为默认密码策略
        if (id.equals(getDefaultPasswordPolicy().getPolicyId())) {
            throw new BizException(MessageConstants.PWD_POLICY_MSG_1008);
        }

        // 2. 删除密码策略
        super.remove(Wrappers.<PasswordPolicy>lambdaQuery().eq(PasswordPolicy::getPolicyId, id));

        // 3. 删除密码策略应用主体
        passwordPolicyMappingService.remove(Wrappers.<PasswordPolicyMapping>lambdaQuery().eq(PasswordPolicyMapping::getPolicyId, id));
    }

    /**
     * 检查密码强度（直接使用密码策略）
     *
     * @param requestDto 请求
     * @return 检查结果
     */
    @Override
    public CheckPasswordStrengthResponseDto checkPasswordStrength(CheckPasswordStrengthWithPolicyRequestDto requestDto) {
        PasswordPolicyRequestDto passwordPolicyRequestDto = requestDto.getPasswordPolicy();
        String password = requestDto.getPassword();
        // 1. 参数校验
        CommonUtil.validateBean(passwordPolicyRequestDto, ValidationGroups.Operation.INSERT.class);

        // 2. 检查密码强度
        PasswordStrength passwordStrength = PasswordStrength.fromOrdinal(passwordPolicyRequestDto.getPasswordStrength());
        // 2.1 自定义密码强度
        if (PasswordStrength.CUSTOM.equals(passwordStrength)) {
            PasswordComplexityConfig complexityConfig = buildComplexityConfig(passwordPolicyRequestDto);
            CustomPasswordStrengthChecker passwordStrengthChecker = (CustomPasswordStrengthChecker) getPasswordStrengthChecker(passwordStrength, complexityConfig);
            Boolean result = passwordStrengthChecker.validate(password);
            var ruleResults = CommonUtil.stream(passwordStrengthChecker.getValidateResult(password)).map(x -> PasswordRuleValidationResultResponseDto.builder()
                    .rule(x._1)
                    .valid(x._2).build()).toList();
            return CheckPasswordStrengthResponseDto.builder()
                    .valid(result)
                    .errorMessage(passwordStrengthChecker.errorMessage())
                    .ruleResults(ruleResults)
                    .build();
        } else {
            // 2.2 其他密码强度
            PasswordStrengthChecker passwordStrengthChecker = getPasswordStrengthChecker(passwordStrength, null);
            Boolean result = passwordStrengthChecker.validate(password);
            return CheckPasswordStrengthResponseDto.builder()
                    .valid(result)
                    .errorMessage(passwordStrengthChecker.errorMessage())
                    .build();
        }
    }

    /**
     * 检查密码强度
     *
     * @param requestDto 请求
     * @return 检查结果
     */
    @Override
    public CheckPasswordStrengthResponseDto checkPasswordStrength(CheckPasswordStrengthRequestDto requestDto) {
        String password = requestDto.getPassword();
        String identity = requestDto.getIdentity();
        // 1. 获取用户匹配的密码策略
        // 1.1 未指定用户标识
        if (StringUtils.isEmpty(identity)) {
            // 1.1.1 从 SecurityContext 中获取
            Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
            if (authentication instanceof UsernamePasswordAuthenticationToken) {
                identity = ((User) authentication.getPrincipal()).getUserId();
            } else {
                // 1.1.2 从 access_token 中获取
                identity = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
            }
        }
        // 1.2 获取密码策略
        User targetUser = getUserByIdentity(identity);
        PasswordPolicy passwordPolicy = passwordPolicyRepository.getMatchedPasswordPolicy(targetUser.getUserId());
        if (Objects.isNull(passwordPolicy)) {
            // 1.3 获取默认密码策略
            passwordPolicy = getDefaultPasswordPolicy();
        }

        // 2. 检查密码强度
        PasswordStrength passwordStrength = PasswordStrength.fromOrdinal(passwordPolicy.getPasswordStrength());
        // 2.1 自定义密码强度
        if (PasswordStrength.CUSTOM.equals(passwordStrength)) {
            PasswordComplexityConfig complexityConfig = CommonUtil.deserializeObject(passwordPolicy.getCustomStrengthConfig(), PasswordComplexityConfig.class);
            CustomPasswordStrengthChecker passwordStrengthChecker = (CustomPasswordStrengthChecker) getPasswordStrengthChecker(passwordStrength, complexityConfig);
            if (Boolean.TRUE.equals(complexityConfig.isProhibitUserInfo())) {
                // 2.1.1 添加当前用户信息
                Optional.ofNullable(targetUser.getUsername()).filter(StringUtils::isNotEmpty).ifPresent(passwordStrengthChecker::addUserInfo);
                Optional.ofNullable(targetUser.getEmailAddress()).filter(StringUtils::isNotEmpty).ifPresent(passwordStrengthChecker::addUserInfo);
                Optional.ofNullable(targetUser.getPhoneNumber()).filter(StringUtils::isNotEmpty).ifPresent(passwordStrengthChecker::addUserInfo);
            }

            // 2.1.2 检查密码强度
            Boolean result = passwordStrengthChecker.validate(password);
            var ruleResults = CommonUtil.stream(passwordStrengthChecker.getValidateResult(password)).map(x -> PasswordRuleValidationResultResponseDto.builder()
                    .rule(x._1)
                    .valid(x._2).build()).toList();
            return CheckPasswordStrengthResponseDto.builder()
                    .valid(result)
                    .errorMessage(passwordStrengthChecker.errorMessage())
                    .ruleResults(ruleResults)
                    .build();
        } else {
            // 2.2 其他密码强度
            PasswordStrengthChecker passwordStrengthChecker = getPasswordStrengthChecker(passwordStrength, null);
            Boolean result = passwordStrengthChecker.validate(password);
            return CheckPasswordStrengthResponseDto.builder()
                    .valid(result)
                    .errorMessage(passwordStrengthChecker.errorMessage())
                    .build();
        }
    }

    /**
     * 检查登录密码强度是否满足要求
     *
     * @param userId 用户 ID
     * @param password 密码
     * @return 是否满足要求
     */
    @Override
    public boolean checkLoginPasswordStrength(String userId, String password) {
        // 1. 获取用户匹配的密码策略
        PasswordPolicy passwordPolicy = passwordPolicyRepository.getMatchedPasswordPolicy(userId);
        if (Objects.isNull(passwordPolicy)) {
            // 1.1 获取默认密码策略
            passwordPolicy = getDefaultPasswordPolicy();
        }

        // 2. 检查密码强度
        // 2.1 未开启登录密码检查
        if (!Boolean.TRUE.equals(passwordPolicy.getEnablePasswordDetection())) {
            return true;
        }
        PasswordStrength passwordStrength = PasswordStrength.fromOrdinal(passwordPolicy.getPasswordStrength());
        // 2.2 自定义密码强度
        if (PasswordStrength.CUSTOM.equals(passwordStrength)) {
            PasswordComplexityConfig complexityConfig = CommonUtil.deserializeObject(passwordPolicy.getCustomStrengthConfig(), PasswordComplexityConfig.class);
            CustomPasswordStrengthChecker passwordStrengthChecker = (CustomPasswordStrengthChecker) getPasswordStrengthChecker(passwordStrength, complexityConfig);
            if (Boolean.TRUE.equals(complexityConfig.isProhibitUserInfo())) {
                // 2.2.1 添加当前用户信息
                User targetUser = userService.getById(userId);
                Optional.ofNullable(targetUser.getUsername()).filter(StringUtils::isNotEmpty).ifPresent(passwordStrengthChecker::addUserInfo);
                Optional.ofNullable(targetUser.getEmailAddress()).filter(StringUtils::isNotEmpty).ifPresent(passwordStrengthChecker::addUserInfo);
                Optional.ofNullable(targetUser.getPhoneNumber()).filter(StringUtils::isNotEmpty).ifPresent(passwordStrengthChecker::addUserInfo);
            }

            // 2.2.2 检查密码强度
            return passwordStrengthChecker.validate(password);
        } else {
            // 2.3 其他密码强度
            return getPasswordStrengthChecker(passwordStrength, null).validate(password);
        }
    }

    /**
     * 检查密码强度
     *
     * @param userId 用户ID
     * @param password 密码
     */
    @Override
    public void checkPasswordStrength(String userId, String password) {
        if (!checkLoginPasswordStrength(userId, password)) {
            throw new BizException(MessageConstants.PWD_POLICY_MSG_1011);
        }
    }

    @SuppressWarnings("all")
    private void checkRequestDto(PasswordPolicyRequestDto requestDto) {
        Integer passwordStrength = requestDto.getPasswordStrength();
        Boolean isUpdate =Objects.nonNull(requestDto.getId());
        if (passwordStrength == PasswordStrength.CUSTOM.ordinal()) {
            CommonUtil.validateBean(requestDto, PasswordPolicyRequestDto.StrengthLevel4.class);

            if (requestDto.getMaxLength() < requestDto.getMinLength()) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1000);
            }

            // 检查字符类型数量
            int chatTypeCount = calculateCharTypeCount(requestDto);
            if (chatTypeCount == 0) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1009);
            }

            // 检查至少需要满足的字符类型数量是否小于等于最小字符类型数量
            if (chatTypeCount < requestDto.getMinCharTypeCount()) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1001);
            }

            // 检查禁止包含的连续字符的最小长度是否小于 2
            if (Boolean.TRUE.equals(requestDto.getProhibitContainConsecutiveChar()) && requestDto.getMinConsecutiveCharLength() < 2) {
                    throw new BizException(MessageConstants.PWD_POLICY_MSG_1002);
            }

            // 检查禁止使用的特定密码列表是否为空
            if (Boolean.TRUE.equals(requestDto.getProhibitSpecificPassword()) && CollectionUtils.isEmpty(requestDto.getProhibitedPasswordList())) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1003);
            }
        }

        if (Boolean.TRUE.equals(requestDto.getEnableForceChangePassword())) {
            CommonUtil.validateBean(requestDto, PasswordPolicyRequestDto.ForceChangePassword.class);
        }

        // 检查策略名称是否已存在
        if (isUpdate) {
            if (super.exists(Wrappers.<PasswordPolicy>lambdaQuery().eq(PasswordPolicy::getPolicyName, requestDto.getName()).ne(PasswordPolicy::getPolicyId, requestDto.getId()))) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1005);
            }
        } else {
            if (super.exists(Wrappers.<PasswordPolicy>lambdaQuery().eq(PasswordPolicy::getPolicyName, requestDto.getName()))) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1005);
            }
        }

        // 检查策略应用主体是否为空（只检查非默认策略）
        if (!isUpdate || (isUpdate && !requestDto.getId().equals(getDefaultPasswordPolicy().getPolicyId()))) {
            if (CollectionUtils.isEmpty(requestDto.getUserIds()) && CollectionUtils.isEmpty(requestDto.getUserGroupIds())) {
                throw new BizException(MessageConstants.PWD_POLICY_MSG_1004);
            }
        }
    }

    private int calculateCharTypeCount(PasswordPolicyRequestDto requestDto) {
        int requireCharTypeCount = 0;
        if (Boolean.TRUE.equals(requestDto.getRequireNumber())) {
            requireCharTypeCount++;
        }
        if (Boolean.TRUE.equals(requestDto.getRequireLowerCase())) {
            requireCharTypeCount++;
        }
        if (Boolean.TRUE.equals(requestDto.getRequireUpperCase())) {
            requireCharTypeCount++;
        }
        if (Boolean.TRUE.equals(requestDto.getRequireSpecialChar())) {
            requireCharTypeCount++;
        }
        return requireCharTypeCount;
    }

    private PasswordComplexityConfig buildComplexityConfig(PasswordPolicyRequestDto requestDto) {
        PasswordComplexityConfig complexityConfig = new PasswordComplexityConfig();
        // 最小长度
        if (Objects.nonNull(requestDto.getMinLength())) {
            complexityConfig.setMinLength(requestDto.getMinLength());
        }
        // 最大长度
        if (Objects.nonNull(requestDto.getMaxLength())) {
            complexityConfig.setMaxLength(requestDto.getMaxLength());
        }
        // 是否必须包含数字
        complexityConfig.setRequireNumber(Boolean.TRUE.equals(requestDto.getRequireNumber()));
        // 是否必须包含小写字母
        complexityConfig.setRequireLowerCase(Boolean.TRUE.equals(requestDto.getRequireLowerCase()));
        // 是否必须包含大写字母
        complexityConfig.setRequireUpperCase(Boolean.TRUE.equals(requestDto.getRequireUpperCase()));
        // 是否必须包含特殊字符
        complexityConfig.setRequireSpecialChar(Boolean.TRUE.equals(requestDto.getRequireSpecialChar()));
        // 至少需要满足的字符类型数量
        if (Objects.nonNull(requestDto.getMinCharTypeCount())) {
            complexityConfig.setMinCharTypeCount(requestDto.getMinCharTypeCount());
        }

        // 是否禁止全部为单一字符
        complexityConfig.setProhibitSingleChar(Boolean.TRUE.equals(requestDto.getProhibitSingleChar()));
        // 是否禁止全部为连续字符
        complexityConfig.setProhibitConsecutiveChar(Boolean.TRUE.equals(requestDto.getProhibitConsecutiveChar()));
        // 是否禁止包含连续字符
        complexityConfig.setProhibitContainConsecutiveChar(Boolean.TRUE.equals(requestDto.getProhibitContainConsecutiveChar()));
        // 禁止包含的连续字符的最小长度
        if (Objects.nonNull(requestDto.getMinConsecutiveCharLength())) {
            complexityConfig.setMinConsecutiveCharLength(requestDto.getMinConsecutiveCharLength());
        }
        // 是否禁止包含连续重复字符
        complexityConfig.setProhibitContainRepeatChar(Boolean.TRUE.equals(requestDto.getProhibitContainRepeatChar()));
        // 禁止包含的连续重复字符的最小长度
        if (Objects.nonNull(requestDto.getMinRepeatCharLength())) {
            complexityConfig.setMinRepeatCharLength(requestDto.getMinRepeatCharLength());
        }

        // 是否禁止包含用户信息
        complexityConfig.setProhibitUserInfo(Boolean.TRUE.equals(requestDto.getProhibitUserInfo()));
        // 是否禁止使用特定密码
        complexityConfig.setProhibitSpecificPassword(Boolean.TRUE.equals(requestDto.getProhibitSpecificPassword()));
        // 禁止使用的特定密码列表
        complexityConfig.setProhibitedPasswordList(requestDto.getProhibitedPasswordList());
        return complexityConfig;
    }

    /**
     * 获取默认密码策略
     *
     * @return 默认密码策略
     */
    public PasswordPolicy getDefaultPasswordPolicy() {
        return super.getOne(Wrappers.<PasswordPolicy>lambdaQuery().eq(PasswordPolicy::getPolicyName, DEFAULT_POLICY_NAME));
    }

    private List<PasswordPolicyMapping> getMappings(PasswordPolicyRequestDto requestDto, String policyId) {
        List<PasswordPolicyMapping> mappings = new ArrayList<>();
        mappings.addAll(
                CommonUtil.stream(requestDto.getUserIds()).map(userId -> {
                    PasswordPolicyMapping mapping = new PasswordPolicyMapping();
                    mapping.setPolicyId(policyId);
                    mapping.setUserId(userId);
                    return mapping;
                }).toList()
        );
        mappings.addAll(
                CommonUtil.stream(requestDto.getUserGroupIds()).map(userGroupId -> {
                    PasswordPolicyMapping mapping = new PasswordPolicyMapping();
                    mapping.setPolicyId(policyId);
                    mapping.setUserGroupId(userGroupId);
                    return mapping;
                }).toList()
        );
        return mappings;
    }

    private PasswordStrengthChecker getPasswordStrengthChecker(PasswordStrength passwordStrength, PasswordComplexityConfig complexityConfig) {
        return switch (passwordStrength) {
            case LOW -> new LowPasswordStrengthPasswordChecker();
            case MEDIUM -> new MediumPasswordStrengthChecker();
            case HIGH -> new HighPasswordStrengthChecker();
            case CUSTOM -> new CustomPasswordStrengthChecker(complexityConfig);
            default -> new NonePasswordStrengthChecker();
        };
    }

    private User getUserByIdentity(String identity) {
        User user =  userService.getOne(Wrappers.<User>lambdaQuery()
                .eq(User::getUserId, identity).or()
                .eq(User::getPhoneNumber, identity).or()
                .eq(User::getEmailAddress, identity).or()
                .eq(User::getUsername, identity));

        if (Objects.isNull(user)) {
            throw new BizException(MessageConstants.PWD_POLICY_MSG_1010);
        }

        return user;
    }
}
