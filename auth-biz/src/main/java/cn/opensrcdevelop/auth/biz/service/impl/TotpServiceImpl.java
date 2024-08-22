package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.TotpCodeCheckRequestDto;
import cn.opensrcdevelop.auth.biz.dto.TotpCodeCheckResponseDto;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.mfa.MultiFactorAuthenticator;
import cn.opensrcdevelop.auth.biz.mfa.TotpValidContext;
import cn.opensrcdevelop.auth.biz.service.TotpService;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.common.exception.BizException;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TotpServiceImpl implements TotpService {

    private final UserService userService;

    /**
     * 一次性密码校验
     *
     * @param requestDto 请求
     * @return 检验结果
     */
    @Override
    public TotpCodeCheckResponseDto check(TotpCodeCheckRequestDto requestDto, HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        TotpValidContext totpValidContext = (TotpValidContext) session.getAttribute(AuthConstants.TOTP_VALID_CONTEXT);

        // 1. 获取用户 MFA 密钥及设备绑定状态
        User user = userService.getOne(Wrappers.<User>lambdaQuery().select(User::getMfaSecret, User::getMfaDeviceBind).eq(User::getUserId, totpValidContext.getUserId()));
        String secret = user.getMfaSecret();
        if (StringUtils.isEmpty(secret)) {
            throw new BizException(MessageConstants.TOTP_MSG_1000);
        }

        // 2. 校验
        boolean checkRes = MultiFactorAuthenticator.checkCode(secret, requestDto.getCode(), System.currentTimeMillis());

        // 3. 设置校验结果
        if (checkRes) {
            totpValidContext.setValid(true);
            session.setAttribute(AuthConstants.TOTP_VALID_CONTEXT, totpValidContext);
        }

        // 4. 初次绑定，执行校验操作后，更新设备绑定状态为已绑定
        if (BooleanUtils.isNotTrue(user.getMfaDeviceBind())) {
            userService.update(Wrappers.<User>lambdaUpdate().set(User::getMfaDeviceBind, true).eq(User::getUserId, totpValidContext.getUserId()));
        }

        TotpCodeCheckResponseDto responseDto = new TotpCodeCheckResponseDto();
        responseDto.setValid(checkRes);
        return responseDto;
    }
}
