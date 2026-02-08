package cn.opensrcdevelop.auth.biz.service.auth.impl;

import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.audit.util.AuditUtil;
import cn.opensrcdevelop.auth.biz.component.authserver.UserTokenBasedRememberMeServices;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.auth.TotpCodeCheckRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.TotpCodeCheckResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mfa.MfaValidContext;
import cn.opensrcdevelop.auth.biz.mfa.TotpAuthenticator;
import cn.opensrcdevelop.auth.biz.service.auth.TotpService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.exception.BizException;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TotpServiceImpl implements TotpService {

    private final UserService userService;
    private final UserTokenBasedRememberMeServices rememberMeServices;

    /**
     * 一次性密码校验
     */
    @Override
    public TotpCodeCheckResponseDto check(TotpCodeCheckRequestDto requestDto, HttpServletRequest request,
            HttpServletResponse response) {
        HttpSession session = request.getSession(false);
        MfaValidContext mfaValidContext = (MfaValidContext) session.getAttribute(AuthConstants.MFA_VALID_CONTEXT);

        // 1. 获取用户 TOTP 密钥及设备绑定状态
        User user = userService.getOne(Wrappers.<User>lambdaQuery().select(User::getTotpSecret, User::getTotpDeviceBind)
                .eq(User::getUserId, mfaValidContext.getUserId()));
        String secret = user.getTotpSecret();
        if (StringUtils.isEmpty(secret)) {
            throw new BizException(MessageConstants.TOTP_MSG_1000);
        }

        // 2. 校验
        boolean checkRes = TotpAuthenticator.checkCode(secret, requestDto.getCode(), System.currentTimeMillis());

        // 3. 设置校验结果
        if (checkRes) {
            mfaValidContext.addValidatedMethod(AuthConstants.MFA_METHOD_TOTP);
            mfaValidContext.setValid(true);
            session.setAttribute(AuthConstants.MFA_VALID_CONTEXT, mfaValidContext);

            // 3.1 设置 RememberToken
            if (mfaValidContext.isRememberMeRequested()) {
                rememberMeServices.setRememberMeTokenToCookie(request, response, user.getUserId());
            }
        }

        // 4. 初次绑定，执行校验操作后，更新设备绑定状态为已绑定
        if (BooleanUtils.isNotTrue(user.getTotpDeviceBind())) {
            userService.update(Wrappers.<User>lambdaUpdate().set(User::getTotpDeviceBind, true).eq(User::getUserId,
                    mfaValidContext.getUserId()));

            // 5. 审计
            AuditUtil.publishSuccessUserOperationAuditEvent(AuthUtil.getCurrentUserId(), ResourceType.USER,
                    UserOperationType.BIND_MFA, "绑定了 MFA 设备");
        }

        TotpCodeCheckResponseDto responseDto = new TotpCodeCheckResponseDto();
        responseDto.setValid(checkRes);
        return responseDto;
    }
}
