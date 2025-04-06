package cn.opensrcdevelop.auth.biz.task;

import cn.opensrcdevelop.auth.biz.entity.*;
import cn.opensrcdevelop.auth.biz.service.*;
import cn.opensrcdevelop.auth.biz.service.impl.PasswordPolicyServiceImpl;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.component.MultiTenantProperties;
import cn.opensrcdevelop.tenant.entity.Tenant;
import cn.opensrcdevelop.tenant.service.TenantService;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

@RequiredArgsConstructor
@Component
@Slf4j
public class UpdatePasswordRemindTask {

    private final PasswordPolicyService passwordPolicyService;
    private final PasswordPolicyMappingService passwordPolicyMappingService;
    private final UserGroupMappingService userGroupMappingService;
    private final UserService userService;
    private final MailService mailService;
    private final UpdatePasswordRemindLogService updatePasswordRemindLogService;
    private final TenantService tenantService;

    @Scheduled(cron = "${auth.server.update-pwd-remind-task-cron}")
    public void execute() {
        // 1. 获取全部租户
        var tenants = tenantService.list(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getEnabled, Boolean.TRUE));
        // 1.1 添加默认租户
        Tenant defaultTenant = new Tenant();
        defaultTenant.setTenantCode(SpringContextUtil.getBean(MultiTenantProperties.class).getDefaultTenant());
        tenants.addFirst(defaultTenant);

        for (var tenant : tenants) {
            try {
                // 2. 执行租户修改密码提醒定时任务
                // 2.1 设置线程上下文
                String tenantCode = tenant.getTenantCode();
                TenantContext tenantContext = new TenantContext();
                tenantContext.setTenantCode(tenantCode);
                TenantContextHolder.setTenantContext(tenantContext);
                MDC.put(CommonConstants.MDC_TENANT_CODE, tenantCode);

                // 2.2 切换租户数据源
                TenantHelper.switchTenantDs(tenantCode);

                // 2.3 执行
                log.info("开始执行修改密码提醒定时任务");
                doExecute();
                log.info("结束执行修改密码提醒定时任务");
            } catch (Exception e) {
                log.error("执行修改密码提醒定时任务失败", e);
            } finally {
                TenantContextHolder.removeTenantContext();
            }
        }
    }

    private void doExecute() {
        Map<PasswordPolicy, List<String>> passwordPolicyWithUserList = new HashMap<>();
        LocalDateTime executeTime = LocalDateTime.now();

        // 1. 获取所有开启密码到期提醒的密码策略
        List<PasswordPolicy> passwordPolicyList =  passwordPolicyService.list(Wrappers.<PasswordPolicy>lambdaQuery()
                .eq(PasswordPolicy::getEnableForceChangePassword, true)
                .and(o -> o.eq(PasswordPolicy::getEnabled, true))
                .orderByAsc(PasswordPolicy::getPriority));

        // 2.获取密码策略应用主体
        Set<String> processedUserIds = new HashSet<>();
        CommonUtil.stream(passwordPolicyList).forEach(passwordPolicy -> {
            List<PasswordPolicyMapping> passwordPolicyMappings = passwordPolicyMappingService.list(Wrappers.<PasswordPolicyMapping>lambdaQuery().eq(PasswordPolicyMapping::getPolicyId, passwordPolicy.getPolicyId()));
            List<String> userList = new ArrayList<>();
            CommonUtil.stream(passwordPolicyMappings).forEach(passwordPolicyMapping -> {
                // 2.1 主体类型为用户
                if (StringUtils.isNotEmpty(passwordPolicyMapping.getUserId()) && !processedUserIds.contains(passwordPolicyMapping.getUserId())) {
                    processedUserIds.add(passwordPolicyMapping.getUserId());
                    userList.add(passwordPolicyMapping.getUserId());
                }

                // 2.2 主体类型为用户组
                if (StringUtils.isNotEmpty(passwordPolicyMapping.getUserGroupId())) {
                    // 2.2.1 获取用户组下的所有用户
                    List<UserGroupMapping> userGroupMappings = userGroupMappingService.list(Wrappers.<UserGroupMapping>lambdaQuery().eq(UserGroupMapping::getUserGroupId, passwordPolicyMapping.getUserGroupId()));
                    List<String> userIds = CommonUtil.stream(userGroupMappings)
                            .map(UserGroupMapping::getUserId)
                            .filter(userId -> !processedUserIds.contains(userId))
                            .toList();
                    processedUserIds.addAll(userIds);
                    userList.addAll(userIds);
                }
            });
            passwordPolicyWithUserList.put(passwordPolicy, CommonUtil.stream(userList).distinct().collect(Collectors.toList()));
        });

        // 3. 向默认密码策略添加所有用户
        fillDefaultPasswordPolicyUserIds(passwordPolicyWithUserList, processedUserIds);

        // 4. 遍历密码策略
        for (Map.Entry<PasswordPolicy, List<String>> entry : passwordPolicyWithUserList.entrySet()) {
            PasswordPolicy passwordPolicy = entry.getKey();
            List<String> userList = entry.getValue();
            if (CollectionUtils.isEmpty(userList)) {
                log.info("密码策略: [{}] 下没有用户", passwordPolicy.getPolicyName());
                continue;
            }

            // 4.1 获取所有用户的最后修改密码时间
            List<User> users = userService.list(Wrappers.<User>lambdaQuery()
                    .select(User::getUserId, User::getLastUpdatePasswordTime, User::getUsername, User::getEmailAddress, User::getVersion)
                    .eq(User::getNeedChangePwd, false)
                    .and(o -> o.in(User::getUserId, userList))
            );

            // 4.2 获取密码已过期的用户
            List<User> expiredUsers = CommonUtil.stream(users).filter(user -> isPasswordExpired(passwordPolicy, executeTime, user.getLastUpdatePasswordTime())).toList();

            // 4.3 更新用户需要修改密码标记
            if (CollectionUtils.isNotEmpty(expiredUsers)) {
                log.info("密码策略: [{}] 下的用户: {} 密码已过期", passwordPolicy.getPolicyName(), CommonUtil.stream(expiredUsers).map(User::getUserId).toList());
                updateUserNeedChangePwd(expiredUsers);
                users.removeAll(expiredUsers);
            }

            // 4.4 获取密码即将过期的用户
            List<User> soonExpiredUsers = CommonUtil.stream(users).filter(user -> isPasswordSoonExpired(passwordPolicy, executeTime, user.getLastUpdatePasswordTime())).toList();

            // 4.5 发送提醒修改密码邮件
            if (CollectionUtils.isNotEmpty(soonExpiredUsers)) {
                log.info("密码策略: [{}] 下的用户: {} 密码即将过期", passwordPolicy.getPolicyName(), CommonUtil.stream(soonExpiredUsers).map(User::getUserId).toList());
                remindUpdatePassword(soonExpiredUsers, passwordPolicy, executeTime);
            }
        }
    }

    /**
     * 向默认密码策略添加所有用户
     *
     * @param passwordPolicyWithUserList 密码策略与用户列表
     * @param processedUserIds 已处理的用户ID
     */
    private void fillDefaultPasswordPolicyUserIds(Map<PasswordPolicy, List<String>> passwordPolicyWithUserList, Set<String> processedUserIds) {
        // 1. 获取默认密码策略
        PasswordPolicy defaultPasswordPolicy = ((PasswordPolicyServiceImpl) passwordPolicyService).getDefaultPasswordPolicy();
        CommonUtil.stream(passwordPolicyWithUserList.entrySet())
                .filter(entry -> defaultPasswordPolicy.getPolicyId().equals(entry.getKey().getPolicyId()))
                .forEach(entry -> {
                    // 2. 获取全部用户ID
                    List<User> allUsers = userService.list(Wrappers.<User>lambdaQuery().select(User::getUserId).eq(User::getLocked, false));
                    entry.getValue().addAll(CommonUtil.stream(allUsers).map(User::getUserId).filter(userId -> !processedUserIds.contains(userId)).toList());
                });
    }

    /**
     * 判断用户密码是否过期
     *
     * @param passwordPolicy 密码策略
     * @param executeTime 执行时间
     * @param lastPasswordUpdateTime 最后修改密码时间
     * @return 是否过期
     */
    private boolean isPasswordExpired(PasswordPolicy passwordPolicy, LocalDateTime executeTime, LocalDateTime lastPasswordUpdateTime) {
        if (Objects.isNull(lastPasswordUpdateTime)) {
            return true;
        }

        // 1. 添加密码周期
        LocalDateTime passwordExpirationTime = lastPasswordUpdateTime.plus(passwordPolicy.getForcedCycle(), convert2ChronoUnit(passwordPolicy.getForcedCycleUnit()));
        // 2. 判断密码是否过期
        return !executeTime.isBefore(passwordExpirationTime);
    }

    /**
     * 转换时间单位
     *
     * @param unit 时间单位
     * @return 转换后的时间单位
     */
    private ChronoUnit convert2ChronoUnit(String unit) {
        return switch (unit) {
            case "DAY" -> ChronoUnit.DAYS;
            case "MONTH" -> ChronoUnit.MONTHS;
            case "YEAR" -> ChronoUnit.YEARS;
            default -> throw new IllegalArgumentException("不支持的时间单位: " + unit);
        };
    }

    /**
     * 更新用户需要修改密码标记
     *
     * @param users 用户列表
     */
    private void updateUserNeedChangePwd(List<User> users) {
        List<User> updateUsers = CommonUtil.stream(users).map(user -> {
            User updateUser = new User();
            updateUser.setUserId(user.getUserId());
            updateUser.setNeedChangePwd(true);
            updateUser.setUpdateBy(this.getClass().getSimpleName());
            updateUser.setVersion(user.getVersion());

            return updateUser;
        }).toList();

        userService.updateBatchById(updateUsers);
    }

    /**
     * 判断用户密码是否即将过期
     *
     * @param passwordPolicy 密码策略
     * @param executeTime 执行时间
     * @param lastPasswordUpdateTime 最后修改密码时间
     * @return 是否即将过期
     */
    private boolean isPasswordSoonExpired(PasswordPolicy passwordPolicy, LocalDateTime executeTime, LocalDateTime lastPasswordUpdateTime) {
        if (Objects.isNull(lastPasswordUpdateTime)) {
            return true;
        }

        // 1. 获取密码过期时间
        LocalDateTime passwordExpirationTime = lastPasswordUpdateTime.plus(passwordPolicy.getForcedCycle(), convert2ChronoUnit(passwordPolicy.getForcedCycleUnit()));

        // 2. 减去密码提醒周期（提醒开始时间）
        LocalDateTime passwordRemindTime = passwordExpirationTime.minus(passwordPolicy.getRemindCycle(), convert2ChronoUnit(passwordPolicy.getRemindCycleUnit()));

        // 3. 判断密码是否即将过期
        return !executeTime.isBefore(passwordRemindTime) && executeTime.isBefore(passwordExpirationTime);
    }

    /**
     * 发送修改密码提醒邮件
     *
     * @param users 用户列表
     * @param passwordPolicy 密码策略
     * @param executeTime 执行时间
     */
    private void remindUpdatePassword(List<User> users, PasswordPolicy passwordPolicy, LocalDateTime executeTime) {
        for (User user : users) {
            // 若当天已经提醒过，则不再提醒
            List<UpdatePasswordRemindLog> remindLogs = updatePasswordRemindLogService.list(Wrappers.<UpdatePasswordRemindLog>lambdaQuery()
                    .eq(UpdatePasswordRemindLog::getUserId, user.getUserId())
                    .eq(UpdatePasswordRemindLog::getPolicyId, passwordPolicy.getPolicyId())
                    .eq(UpdatePasswordRemindLog::isSuccess, true)
                    .between(UpdatePasswordRemindLog::getRemindTime, executeTime.toLocalDate().atStartOfDay(), executeTime.toLocalDate().atTime(LocalTime.MAX))
            );

            if (CollectionUtils.isNotEmpty(remindLogs)) {
                log.debug("用户: [{}] 已经在当天发送过修改密码提醒邮件", user.getUserId());
                continue;
            }

            String email = user.getEmailAddress();
            if (StringUtils.isEmpty(email)) {
                log.info("用户: [{}] 未绑定邮箱，无法发送修改密码提醒邮件", user.getUserId());
                return;
            }
            mailService.sendRemindUpdatePwd(user, passwordPolicy, user.getLastUpdatePasswordTime().plus(passwordPolicy.getRemindCycle(), convert2ChronoUnit(passwordPolicy.getRemindCycleUnit())), executeTime);
        }
    }
}
