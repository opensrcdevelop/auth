package cn.opensrcdevelop.auth.biz.service.user.impl;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.dto.user.LoginLogResponseDto;
import cn.opensrcdevelop.auth.biz.entity.client.Client;
import cn.opensrcdevelop.auth.biz.entity.user.LoginLog;
import cn.opensrcdevelop.auth.biz.mapper.user.LoginLogMapper;
import cn.opensrcdevelop.auth.biz.service.client.ClientService;
import cn.opensrcdevelop.auth.biz.service.user.LoginLogService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.springframework.aop.framework.AopContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class LoginLogServiceImpl extends ServiceImpl<LoginLogMapper, LoginLog> implements LoginLogService {

    private static final String PROP_MAX_LOGIN_LOGIN_NUM = "auth.server.max-login-log-num";
    private static final String PG_LIMIT = "limit ";

    private final ClientService clientService;

    /**
     * 保存登录日志
     *
     * @param userId 用户ID
     */
    @Override
    public void saveLoginLog(String userId) {
        LoginLogServiceImpl aopSvc = (LoginLogServiceImpl) AopContext.currentProxy();
        aopSvc.saveLoginLog(userId, Integer.parseInt(SpringContextUtil.getProperty(PROP_MAX_LOGIN_LOGIN_NUM)));
    }

    /**
     * 保存登录日志
     *
     * @param userId 用户ID
     * @param maxLoginLogTotalNum 保存的最大登录日志数
     */
    @Transactional
    @Override
    public void saveLoginLog(String userId, Integer maxLoginLogTotalNum) {
        // 1. 保存登录日志
        LoginLog loginLog = new LoginLog();
        String loginLogId = CommonUtil.getUUIDV7String();
        loginLog.setLoginId(loginLogId);
        loginLog.setUserId(userId);
        WebUtil.getRequest().ifPresent(request -> {
            HttpSession session = request.getSession(false);
            if (Objects.nonNull(session)) {
                loginLog.setSessionId(session.getId());
                // 1.1 保存登录日志ID到 Session 中
                session.setAttribute(AuthConstants.SESSION_LOGIN_ID, loginLogId);
            }
        });
        String ipAddress = WebUtil.getRemoteIP();
        loginLog.setLoginIp(ipAddress);
        loginLog.setLoginIpRegion(WebUtil.getIpRegion(ipAddress));
        loginLog.setDeviceType(WebUtil.getDeviceType());
        loginLog.setDeviceOs(WebUtil.getDeviceOs());
        loginLog.setBrowserType(WebUtil.getBrowserType());
        loginLog.setLoginTime(LocalDateTime.now());
        // 1.2 数据库操作
        super.save(loginLog);

        // 2. 删除超过最大登录日志数的登录日志
        // 2.1 获取登录日志数
        long loginLogNum = super.count(Wrappers.<LoginLog>lambdaQuery().eq(LoginLog::getUserId, userId));
        if (Objects.nonNull(maxLoginLogTotalNum) && maxLoginLogTotalNum > 0 && loginLogNum > maxLoginLogTotalNum) {
            // 2.2 获取待删除的登录日志
            List<LoginLog> deleteTargets = super.list(Wrappers.<LoginLog>lambdaQuery().eq(LoginLog::getUserId, userId).orderByAsc(LoginLog::getLoginTime).last(PG_LIMIT + (loginLogNum - maxLoginLogTotalNum)));
            // 2.3 数据库操作
            super.removeBatchByIds(CommonUtil.stream(deleteTargets).map(LoginLog::getLoginId).toList());
        }
    }

    /**
     * 获取最后一次登录信息
     *
     * @param userId 用户ID
     * @return 登录信息
     */
    @Override
    public Tuple4<String, String, String, LocalDateTime> getLastLoginInfo(String userId) {
        LoginLog loginLog = super.getOne(Wrappers.<LoginLog>lambdaQuery().eq(LoginLog::getUserId, userId).orderByDesc(LoginLog::getLoginTime).last(PG_LIMIT + 1));
        if (Objects.nonNull(loginLog)) {
            return Tuple.of(loginLog.getLoginIp(), loginLog.getDeviceType(), loginLog.getDeviceOs(), loginLog.getLoginTime());
        }
        return Tuple.of(null, null, null, null);
    }

    /**
     * 获取用户登录日志列表
     *
     * @param page 页数
     * @param size 条数
     * @param userId 用户ID
     * @return 用户登录日志列表
     */
    @Override
    public PageData<LoginLogResponseDto> getUserLoginLogs(int page, int size, String userId) {
        // 1. 查询数据库
        Page<LoginLog> pageRequest = new Page<>(page, size);
        List<LoginLog> loginLogs = super.list(pageRequest, Wrappers.<LoginLog>lambdaQuery().eq(LoginLog::getUserId, userId).orderByDesc(LoginLog::getLoginTime));

        // 2. 属性编辑
        PageData<LoginLogResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setSize(pageRequest.getSize());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setList(CommonUtil.stream(loginLogs).map(loginLog -> {
            LoginLogResponseDto loginLogResponse = new LoginLogResponseDto();
            loginLogResponse.setLoginId(loginLog.getLoginId());
            loginLogResponse.setLoginIp(loginLog.getLoginIp());
            loginLogResponse.setLoginIpRegion(loginLog.getLoginIpRegion());
            loginLogResponse.setDeviceType(loginLog.getDeviceType());
            loginLogResponse.setDeviceOs(loginLog.getDeviceOs());
            loginLogResponse.setBrowserType(loginLog.getBrowserType());
            loginLogResponse.setLoginTime(loginLog.getLoginTime());

            // 2.1 设置客户端信息
            String clientId = loginLog.getClientId();
            Client client = clientService.getById(clientId);
            if (Objects.nonNull(clientId)) {
                loginLogResponse.setClientId(clientId);
                loginLogResponse.setClientName(client.getClientName());
            }

            return loginLogResponse;
        }).toList());

        return pageData;
    }

    /**
     * 移除用户的最近登录会话
     *
     * @param userId 用户ID
     */
    @Override
    public void removeRecentLoginSessions(String userId) {
        // 1. 获取用户最近一周的登录日志
        LocalDateTime oneWeekAgo = LocalDateTime.now().minusWeeks(1);
        List<LoginLog> loginLogs = super.list(Wrappers.<LoginLog>lambdaQuery()
                .eq(LoginLog::getUserId, userId)
                .ge(LoginLog::getLoginTime, oneWeekAgo)
                .orderByDesc(LoginLog::getLoginTime));

        // 2. 删除用户的最近登录会话
        CommonUtil.stream(loginLogs).forEach(loginLog -> WebUtil.removeSession(loginLog.getSessionId()));
    }
}
