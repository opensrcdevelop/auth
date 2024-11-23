package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.PrincipalTypeEnum;
import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.*;
import cn.opensrcdevelop.auth.biz.entity.*;
import cn.opensrcdevelop.auth.biz.mapper.UserMapper;
import cn.opensrcdevelop.auth.biz.repository.AuthorizationRepository;
import cn.opensrcdevelop.auth.biz.repository.RoleRepository;
import cn.opensrcdevelop.auth.biz.repository.UserRepository;
import cn.opensrcdevelop.auth.biz.service.*;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.JwtUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.nimbusds.jwt.JWTClaimNames;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.Tuple4;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.aop.framework.AopContext;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ReflectionUtils;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService, UserDetailsService {

    private static final String ACCOUNT_NOT_EXISTS = "账号不存在";

    private final PasswordEncoder passwordEncoder;
    private final PermissionService permissionService;
    private final RoleService roleService;
    private final UserAttrService userAttrService;
    private final UserRepository userRepository;
    private final UserGroupService userGroupService;
    private final RoleRepository roleRepository;
    private final EmailService emailService;
    private final AuthorizeService authorizeService;
    private final AuthorizationRepository authorizationRepository;
    private final VerificationCodeService verificationCodeService;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        User user = super.getOne(Wrappers.<User>lambdaQuery().or(o -> o.eq(User::getUsername, username)).or(o -> o.eq(User::getPhoneNumber, username)).or(o -> o.eq(User::getEmailAddress, username)));
        return Optional.ofNullable(user).orElseThrow(() -> new UsernameNotFoundException(ACCOUNT_NOT_EXISTS));
    }

    /**
     * 创建用户
     *
     * @param requestDto 创建用户请求
     */
    @Transactional
    @Override
    public void createUser(UserRequestDto requestDto) {
        // 1. 检查用户名是否存在
        checkUsername(requestDto, null);

        // 2. 检查邮箱地址是否存在
        checkEmailAddress(requestDto, null);

        // 3. 检查手机号码是否存在
        checkPhoneNumber(requestDto, null);

        // 4. 属性设置
        String userId = CommonUtil.getUUIDString();
        User user = new User();
        user.setUserId(userId);
        user.setUsername(requestDto.getUsername());
        user.setPassword(passwordEncoder.encode(requestDto.getPassword()));
        user.setPhoneNumber(requestDto.getPhoneNumber());
        user.setEmailAddress(requestDto.getEmailAddress());
        user.setNeedChangePwd(requestDto.getNeedChangePwd());

        // 5. 插入数据库
        super.save(user);

        // 6. 设置用户扩展属性
        userAttrService.createUserAttrMapping(userId, requestDto.getAttributes());

        // 7. 发送通知邮件
        if (Boolean.TRUE.equals(requestDto.getSendEmail()) && StringUtils.isNotBlank(requestDto.getEmailAddress())) {
            emailService.sendCreateUserNotice(requestDto.getEmailAddress(), requestDto.getUsername(), requestDto.getPassword());
        }
    }

    /**
     * 获取用户信息
     *
     * @param userId            用户ID
     * @param resourceGroupCode 资源组码
     * @return 用户信息
     */
    @Override
    public User getUserInfo(String userId, String resourceGroupCode) {
        // 1. 获取用户信息
        User user = super.getById(userId);
        user.setPassword(null);

        // 2. 设置用户角色信息
        var roles = roleService.getUserRoles(userId);
        // 2.1 去除重复角色信息（同时授予角色主体和用户）
        user.setRoles(CommonUtil.stream(roles).collect(Collectors.collectingAndThen(Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(Role::getRoleCode))), ArrayList::new)));

        // 3. 设置用户属性
        user.setUserAttrs(userAttrService.getUserAttrs(userId));

        // 4. 设置用户组信息
        user.setUserGroups(userGroupService.getUserGroups(userId));
        return user;
    }

    /**
     * 获取用户列表
     *
     * @param page    页数
     * @param size    条数
     * @param filters 过滤条件
     * @return 用户列表
     */
    @Override
    public PageData<Map<String, Object>> list(int page, int size, List<DataFilterRequestDto> filters) {
        // 1. 查询数据库
        Page<User> pageRequest = new Page<>(page, size);

        // 1.1 编辑查询条件
        QueryWrapper<User> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(CommonUtil.extractFileNameFromGetter(User::getDeleted), false);
        if (CollectionUtils.isNotEmpty(filters)) {
            for (DataFilterRequestDto filter : filters) {
                AuthUtil.editQuery(queryWrapper, filter);
            }
            userRepository.searchUsers(pageRequest, queryWrapper);
        } else {
            userRepository.searchUsers(pageRequest, queryWrapper);
        }

        // 2. 属性编辑
        PageData<Map<String, Object>> pageData = new PageData<>();
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setSize(pageRequest.getSize());
        List<Map<String, Object>> userResponses = CommonUtil.stream(pageRequest.getRecords()).map(r -> {
            var userMap = AuthUtil.convertUserMap(r);
            // 2.1 移除用户 Map 中角色信息
            userMap.remove(CommonConstants.ROLES);
            return userMap;
        }).toList();
        pageData.setList(userResponses);

        return pageData;
    }

    /**
     * 更新用户信息
     *
     * @param requestDto 更新用户信息请求
     */
    @Transactional
    @Override
    public void updateUser(UserRequestDto requestDto) {
        String userId = requestDto.getUserId();
        // 1. 获取版本号
        var rawUser = super.getById(userId);
        if (Objects.isNull(rawUser)) {
            return;
        }

        // 2. 检查用户名是否存在
        checkUsername(requestDto, rawUser);

        // 3. 检查邮箱地址是否存在
        checkEmailAddress(requestDto, rawUser);

        // 4. 检查手机号码是否存在
        checkPhoneNumber(requestDto, rawUser);

        // 5. 更新普通属性
        User updateUser = new User();
        updateUser.setUserId(userId);
        updateUser.setEnableMfa(requestDto.getEnableMfa());
        updateUser.setNeedChangePwd(requestDto.getNeedChangePwd());
        updateUser.setLocked(requestDto.getLocked());
        updateUser.setConsoleAccess(requestDto.getConsoleAccess());
        updateUser.setVersion(rawUser.getVersion());
        CommonUtil.callSetWithCheck(StringUtils::isNotBlank, updateUser::setUsername, requestDto::getUsername);
        CommonUtil.callSetWithCheck(StringUtils::isNotBlank, updateUser::setPhoneNumber, requestDto::getPhoneNumber);
        CommonUtil.callSetWithCheck(StringUtils::isNotBlank, updateUser::setEmailAddress, requestDto::getEmailAddress);

        String password = requestDto.getPassword();
        if (StringUtils.isNotBlank(password)) {
            updateUser.setPassword(passwordEncoder.encode(password));
        }
        super.updateById(updateUser);

        // 6. 更新扩展属性
        userAttrService.updateUserUserAttrMapping(userId, requestDto.getAttributes());

        // 7. 发送重置密码通知邮件
        if (StringUtils.isNotBlank(password) && Boolean.TRUE.equals(requestDto.getSendEmail())) {
            // 7.1 获取用户信息
            User user = super.getById(userId);
            String email = user.getEmailAddress() != null ? user.getEmailAddress() : requestDto.getEmailAddress();
            if (StringUtils.isNotBlank(email)) {
                emailService.sendResetPwdNotice(email, user.getUsername(), password);
            }
        }

        // 8. 删除 Token（禁用账号的场合 or 关闭控制台访问的场合）
        if (Boolean.TRUE.equals(requestDto.getLocked()) || Boolean.FALSE.equals(requestDto.getConsoleAccess())) {
            ((UserService) AopContext.currentProxy()).clearAuthorizedTokens(userId);
        }
    }

    /**
     * 获取用户详情
     *
     * @param userId 用户ID
     * @return 用户详情
     */
    @Override
    public UserResponseDto detail(String userId) {
        UserResponseDto userResponse = new UserResponseDto();
        // 1. 获取用户
        User user = super.getById(userId);
        if (user == null) {
            return userResponse;
        }

        // 2. 填充用户信息
        fillUserResponse(user, userResponse);
        return userResponse;
    }

    /**
     * 变更密码
     *
     * @param requestDto 变更密码请求
     */
    @Transactional
    @Override
    public void changePwd(ChangePwdRequestDto requestDto, HttpServletRequest request) {
        String rawPwd = requestDto.getRawPwd();
        String newPwd = requestDto.getNewPwd();

        // 1. 校验
        if (StringUtils.equals(rawPwd, newPwd)) {
            throw new BizException(MessageConstants.LOGIN_MSG_1001);
        }

        // 2. 获取用户
        User user = null;
        // 2.1 从 SecurityContext 中获取
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication instanceof UsernamePasswordAuthenticationToken) {
            user = (User) authentication.getPrincipal();
        } else {
            // 2.2 从 access_token 中获取
            String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
            if (StringUtils.isNotEmpty(userId)) {
                user = super.getById(userId);
            }
        }
        if (Objects.isNull(user)) {
            throw new OAuth2AuthenticationException("invalid authentication");
        }
        if (!passwordEncoder.matches(requestDto.getRawPwd(), user.getPassword())) {
            throw new BizException(MessageConstants.LOGIN_MSG_1002);
        }

        // 3. 更新密码
        User updateUser = new User();
        updateUser.setUserId(user.getUserId());
        updateUser.setPassword(passwordEncoder.encode(newPwd));
        updateUser.setNeedChangePwd(false);
        updateUser.setVersion(user.getVersion());
        super.updateById(updateUser);

        // 4. 移除 SESSION 变更密码标记
        HttpSession session = request.getSession(false);
        if (session != null) {
            session.removeAttribute(AuthConstants.SESSION_CHANGED_PWD);
        }
    }

    /**
     * 删除用户
     *
     * @param userId 用户 ID
     */
    @Transactional
    @Override
    public void removeUser(String userId) {
        // 1. 删除 Token
        ((UserService) AopContext.currentProxy()).clearAuthorizedTokens(userId);

        // 2. 删除用户信息
        super.removeById(userId);

        // 3. 删除角色信息
        roleService.removeUserRoleMapping(userId);

        // 4. 删除授权信息
        authorizeService.removeAuthorization(userId);

        // 5. 删除用户属性
        userAttrService.removeUserAttrMapping(userId);

        // 6. 删除用户组信息
        userGroupService.removeUserGroupMapping(userId);
    }

    /**
     * 获取当前用户信息
     *
     * @return 当前用户信息
     */
    @Override
    public Map<String, Object> getCurrentUserInfo() {
        // 1. 获取当前用户 ID
        String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);

        // 2. 获取当前用户信息
        if (StringUtils.isNotEmpty(userId)) {
            Page<User> pageRequest = new Page<>(1, -1);
            userRepository.searchUsers(pageRequest, Wrappers.<User>query().eq("t1.user_id", userId).eq("deleted", false));
            List<User> queryRes = pageRequest.getRecords();
            if (CollectionUtils.isEmpty(queryRes)) {
                return Collections.emptyMap();
            }

            var userMap = AuthUtil.convertUserMap(queryRes.get(0), true, true);
            // 2.1 获取可见的用户属性
            var visibleUserAttrs = userAttrService.getVisibleUserAttrs();
            // 2.2 删除不可见的用户信息
            var unVisibleUserMapKeys = userMap.keySet().stream().filter(k -> CommonUtil.stream(visibleUserAttrs).noneMatch(attr -> StringUtils.equals(attr.getKey(), k))).collect(Collectors.toSet());
            CommonUtil.stream(unVisibleUserMapKeys).forEach(userMap::remove);
            // 2.3 添加控制台访问权限
            userMap.put(AuthConstants.CONSOLE_ACCESS, queryRes.get(0).getConsoleAccess());
            return userMap;
        }
        return Collections.emptyMap();
    }

    /**
     * 重新绑定 MFA 设备
     *
     * @param userId 用户 ID
     */
    @Transactional
    @Override
    public void rebindMfaDevice(String userId) {
        // 1. 获取版本号
        var rawUser = super.getById(userId);
        if (Objects.isNull(rawUser)) {
            return;
        }
        User updateUser = new User();
        updateUser.setUserId(userId);
        updateUser.setVersion(rawUser.getVersion());

        // 2. 重置 MFA 密钥 & 设备绑定状态
        updateUser.setMfaSecret(StringUtils.EMPTY);
        updateUser.setMfaDeviceBind(false);

        // 3. 数据库操作
        super.updateById(updateUser);
    }

    /**
     * 清空授权的 Token
     *
     * @param userId 用户 ID
     */
    @Transactional
    @Override
    public void clearAuthorizedTokens(String userId) {
        // 1. 检索用户
        User user = super.getById(userId);
        if (Objects.isNull(user)) {
            return;
        }

        // 2. 删除 Token
        authorizationRepository.deleteUserTokens(user.getUsername());
    }

    /**
     * 重置密码
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void resetPwd(ResetPwdRequestDto requestDto) {
        // 1. 校验重置密码 token
        String token = requestDto.getResetPwdToken();
        if (!verificationCodeService.verifyResultToken(token)) {
            throw new BizException(MessageConstants.RESET_PWD_MSG_1000);
        }

        // 2. 获取用户
        String username = (String) JwtUtil.getJwtClaimsWithHS256(token).get(JWTClaimNames.SUBJECT);
        User user = (User) loadUserByUsername(username);
        if (Objects.isNull(user)) {
            throw new BizException(MessageConstants.RESET_PWD_MSG_1000);
        }

        // 3. 更新密码
        User updateUser = new User();
        updateUser.setUserId(user.getUserId());
        updateUser.setPassword(passwordEncoder.encode(requestDto.getNewPwd()));
        updateUser.setVersion(user.getVersion());
        super.updateById(updateUser);
    }

    /**
     * 更新当前用户信息
     *
     * @param userInfo 用户信息
     */
    @Transactional
    @Override
    public void updateMe(Map<String, Object> userInfo) {
        // 1. 获取当前用户 ID
        String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
        if (StringUtils.isBlank(userId)) {
            return;
        }

        // 2. 获取版本号
        var rawUser = super.getById(userId);
        if (Objects.isNull(rawUser)) {
            return;
        }

        // 3. 获取用户可编辑的用户属性
        var editableUserAttrs = CommonUtil.stream(userAttrService.getVisibleUserAttrs()).filter(UserAttrResponseDto::getUserEditable).toList();

        // 4. 编辑属性
        User updateUser = new User();
        updateUser.setUserId(userId);
        updateUser.setVersion(rawUser.getVersion());
        List<UserAttrMappingRequestDto> attributes = new ArrayList<>();
        convertUserInfo(userInfo, editableUserAttrs, updateUser, attributes);

        // 5. 更新扩展属性
        userAttrService.updateUserUserAttrMapping(userId, attributes);

        // 6. 更新用户
        super.updateById(updateUser);
    }

    /**
     * 绑定邮箱
     *
     * @param requestDto 请求
     */
    @Override
    public void bindEmail(BindOrUnbindEmailRequestDto requestDto) {
        doBindOrUnbindEmail(requestDto, true);
    }

    /**
     * 解绑邮箱
     *
     * @param requestDto 请求
     */
    @Override
    public void unbindEmail(BindOrUnbindEmailRequestDto requestDto) {
        doBindOrUnbindEmail(requestDto, false);
    }

    /**
     * 获取权限信息
     *
     * @param page                           页数
     * @param size                           条数
     * @param userId                         用户ID
     * @param resourceGroupNameSearchKeyword 资源组名称搜索关键字
     * @param resourceNameSearchKeyword      资源名称搜索关键字
     * @param permissionNameSearchKeyword    权限名称搜索关键字
     * @param permissionCodeSearchKeyword    权限标识搜索关键字
     * @return 权限信息
     */
    @Override
    public PageData<PermissionResponseDto> getPermissions(int page, int size, String userId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword) {
        // 1. 查询数据库
        Page<AuthorizeRecord> pageRequest = new Page<>(page, size);
        permissionService.getUserPermissions(pageRequest, userId, null, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);

        // 2. 属性编辑
        PageData<PermissionResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        List<PermissionResponseDto> permissionResponseList = CommonUtil.stream(pageRequest.getRecords()).map(authorizeRecord -> {
            PermissionResponseDto permissionResponse = new PermissionResponseDto();
            var permission = authorizeRecord.getPermission();

            // 2.1 权限响应属性
            permissionResponse.setAuthorizeId(authorizeRecord.getAuthorizeId());
            permissionResponse.setPermissionId(permission.getPermissionId());
            permissionResponse.setPermissionName(permission.getPermissionName());
            permissionResponse.setPermissionCode(permission.getPermissionCode());
            permissionResponse.setResourceId(permission.getResource().getResourceId());
            permissionResponse.setResourceCode(permission.getResource().getResourceCode());
            permissionResponse.setResourceName(permission.getResource().getResourceName());
            permissionResponse.setResourceGroupId(permission.getResource().getResourceGroup().getResourceGroupId());
            permissionResponse.setResourceGroupCode(permission.getResource().getResourceGroup().getResourceGroupCode());
            permissionResponse.setResourceGroupName(permission.getResource().getResourceGroup().getResourceGroupName());

            // 2.2 限定条件
            var conditions = CommonUtil.stream(authorizeRecord.getPermissionExps()).map(exp -> {
                PermissionExpResponseDto condition = new PermissionExpResponseDto();
                condition.setId(exp.getExpressionId());
                condition.setName(exp.getExpressionName());
                condition.setExpression(exp.getExpression());
                return condition;
            }).toList();
            permissionResponse.setConditions(conditions);

            // 2.3 被授权主体和主体类型
            var permissionPrincipal = getPermissionPrincipal(authorizeRecord);
            permissionResponse.setPrincipalId(permissionPrincipal._1);
            permissionResponse.setPrincipal(permissionPrincipal._2);
            permissionResponse.setPrincipalType(permissionPrincipal._3);
            permissionResponse.setPrincipalTypeDisplayName(permissionPrincipal._4);
            return permissionResponse;
        }).toList();
        pageData.setList(permissionResponseList);
        return pageData;
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

    private Tuple2<String, String> getRolePrincipal(RoleMapping roleMapping) {
        User user = roleMapping.getUser();
        UserGroup userGroup = roleMapping.getUserGroup();

        if (user != null) {
            return Tuple.of(user.getUsername(), PrincipalTypeEnum.USER.getDisplayName());
        }

        if (userGroup != null) {
            return Tuple.of(userGroup.getUserGroupName(), PrincipalTypeEnum.USER_GROUP.getDisplayName());
        }

        return Tuple.of(null, null);
    }

    private void fillUserResponse(User user, UserResponseDto userResponse) {
        // 1. 基本信息
        userResponse.setId(user.getUserId());
        userResponse.setUsername(user.getUsername());
        userResponse.setPhoneNumber(user.getPhoneNumber());
        userResponse.setEmailAddress(user.getEmailAddress());
        userResponse.setEnableMfa(user.getEnableMfa());
        userResponse.setCreateTime(user.getCreateTime());
        userResponse.setLocked(BooleanUtils.isTrue(user.getLocked()));
        userResponse.setConsoleAccess(BooleanUtils.isTrue(user.getConsoleAccess()));
        userResponse.setLastLoginTime(user.getLastLoginTime());
        userResponse.setLastLoginIp(user.getLastLoginIp());
        userResponse.setLastLoginDeviceType(user.getLastLoginDeviceType());
        userResponse.setLastLoginDeviceOs(user.getLastLoginDeviceOs());

        // 2. 扩展信息
        var attributes = CommonUtil.stream(userAttrService.getUserAttrs(user.getUserId())).map(attr -> {
            UserAttrResponseDto userAttrResponse = new UserAttrResponseDto();
            userAttrResponse.setId(attr.getAttrId());
            userAttrResponse.setKey(attr.getAttrKey());
            userAttrResponse.setValue(AuthUtil.convertUserAttrData(attr.getAttrValue(), UserAttrDataTypeEnum.valueOf(attr.getAttrDataType()), true, true));
            userAttrResponse.setDataType(attr.getAttrDataType());
            userAttrResponse.setExtFlg(attr.getExtAttrFlg());
            return userAttrResponse;
        }).toList();
        userResponse.setAttributes(attributes);

        // 3. 角色信息
        var roles = CommonUtil.stream(roleRepository.searchUserRoles(user.getUserId())).map(roleMapping -> {
            Role role = roleMapping.getRole();

            RoleResponseDto roleResponse = new RoleResponseDto();
            roleResponse.setId(role.getRoleId());
            roleResponse.setName(role.getRoleName());
            roleResponse.setCode(role.getRoleCode());

            // 3.1 角色主体信息
            var rolePrincipal = getRolePrincipal(roleMapping);
            roleResponse.setPrincipal(rolePrincipal._1);
            roleResponse.setPrincipalType(rolePrincipal._2);
            return roleResponse;
        }).toList();
        userResponse.setRoles(roles);

        // 4. 用户组信息
        var userGroups = CommonUtil.stream(userGroupService.getUserGroups(user.getUserId())).map(group -> {
            UserGroupResponseDto userGroupResponse = new UserGroupResponseDto();
            userGroupResponse.setId(group.getUserGroupId());
            userGroupResponse.setName(group.getUserGroupName());
            userGroupResponse.setCode(group.getUserGroupCode());
            return userGroupResponse;
        }).toList();
        userResponse.setUserGroups(userGroups);
    }

    private void convertUserInfo(Map<String, Object> userInfo, List<UserAttrResponseDto> editableUserAttrs, User updateUser, List<UserAttrMappingRequestDto> attributes) {
        CommonUtil.stream(editableUserAttrs).filter(x -> userInfo.containsKey(x.getKey())).forEach(userAttr -> {
            Object userInfoValue = userInfo.get(userAttr.getKey());

            // 4.1 非扩展属性
            if (BooleanUtils.isFalse(userAttr.getExtFlg())) {
                var userField = ReflectionUtils.findField(User.class, userAttr.getKey());
                if (Objects.nonNull(userField)) {
                    ReflectionUtils.makeAccessible(userField);
                    ReflectionUtils.setField(userField, updateUser, userInfoValue);
                }
            }

            // 4.2 扩展属性
            if (BooleanUtils.isTrue(userAttr.getExtFlg())) {
                UserAttrMappingRequestDto userAttrMappingRequest = new UserAttrMappingRequestDto();
                userAttrMappingRequest.setAttrId(userAttr.getId());
                userAttrMappingRequest.setAttrValue(Objects.isNull(userInfoValue) ? null : userInfoValue.toString());
                attributes.add(userAttrMappingRequest);
            }
        });
    }

    private void doBindOrUnbindEmail(BindOrUnbindEmailRequestDto requestDto, boolean isBinding) {
        // 1. 校验验证码
        String email = requestDto.getEmail();
        boolean result = verificationCodeService.verifyCode(email, requestDto.getCode());
        if (!result) {
            throw new BizException(MessageConstants.VERIFY_CODE_MSG_1000);
        }

        // 2. 检查邮箱是否已经被绑定
        if (isBinding && Objects.nonNull(super.getOne(Wrappers.<User>lambdaQuery().eq(User::getEmailAddress, email)))) {
            throw new BizException(MessageConstants.BIND_EMAIL_MSG_1000);
        }

        AuthUtil.getCurrentUserId().ifPresent(userId -> {
            // 3. 获取当前用户
            User rawUser = super.getById(userId);

            // 4. 检查当前用户的邮箱是否与请求的邮箱一致
            if (!isBinding && !StringUtils.equals(email, rawUser.getEmailAddress())) {
                throw new BizException(MessageConstants.UNBIND_EMAIL_MSG_1000);
            }

            // 5. 更新用户信息
            User updateUser = new User();
            updateUser.setUserId(userId);
            updateUser.setVersion(rawUser.getVersion());
            updateUser.setEmailAddress(isBinding ? email : "");
            super.updateById(updateUser);
        });
    }

    private void checkUsername(UserRequestDto requestDto, User rawUser) {
        if (Objects.nonNull(rawUser) && StringUtils.equals(requestDto.getUsername(), rawUser.getUsername())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<User>lambdaQuery().eq(User::getUsername, requestDto.getUsername())))) {
            throw new BizException(MessageConstants.USER_MSG_1000, requestDto.getUsername());
        }
    }

    private void checkEmailAddress(UserRequestDto requestDto, User rawUser) {
        if (Objects.nonNull(rawUser) && StringUtils.equals(requestDto.getEmailAddress(), rawUser.getEmailAddress())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<User>lambdaQuery().eq(User::getEmailAddress, requestDto.getEmailAddress())))) {
            throw new BizException(MessageConstants.USER_MSG_1001, requestDto.getEmailAddress());
        }
    }

    private void checkPhoneNumber(UserRequestDto requestDto, User rawUser) {
        if (Objects.nonNull(rawUser) && StringUtils.equals(requestDto.getPhoneNumber(), rawUser.getPhoneNumber())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<User>lambdaQuery().eq(User::getPhoneNumber, requestDto.getPhoneNumber())))) {
            throw new BizException(MessageConstants.USER_MSG_1002, requestDto.getPhoneNumber());
        }
    }
}
