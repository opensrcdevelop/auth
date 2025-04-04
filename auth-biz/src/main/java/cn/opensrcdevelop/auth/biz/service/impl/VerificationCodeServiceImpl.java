package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.user.CheckCodeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.CheckCodeResponseDto;
import cn.opensrcdevelop.auth.biz.entity.VerificationCode;
import cn.opensrcdevelop.auth.biz.service.VerificationCodeService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.JwtUtil;
import com.nimbusds.jwt.JWTClaimNames;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.Clock;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class VerificationCodeServiceImpl implements VerificationCodeService {

    private final HttpServletRequest httpRequest;

    @Value("${auth.server.jws256}")
    private String jws256;

    @Value("${auth.server.check-code-result-live}")
    private long checkCodeResultLive;

    /**
     * 设置验证码
     *
     * @param receiver 接收方
     * @param liveTime 存活时间
     * @param timeUnit 时间单位
     * @return 验证码
     */
    @Override
    public String setCode(String receiver, long liveTime, ChronoUnit timeUnit) {
        // 1. 生成随机 6 位验证码
        String code =  RandomStringUtils.randomNumeric(6);
        Instant expireTime = Instant.now(Clock.systemDefaultZone()).plus(liveTime, timeUnit);
        VerificationCode verificationCode = new VerificationCode();
        verificationCode.setCode(code);

        // 2. 设置过期时间和接收方
        verificationCode.setExpireTime(expireTime);
        verificationCode.setReceiver(receiver);

        // 3. 设置验证码到 session 中
        HttpSession session = httpRequest.getSession(true);
        session.setAttribute(AuthConstants.VERIFICATION_CODE, verificationCode);
        return code;
    }

    /**
     * 校验验证码
     *
     * @param receiver 接收方
     * @param code 验证码
     * @return 校验结果
     */
    @Override
    public boolean verifyCode(String receiver, String code) {
        // 1. 从 session 获取验证码
        HttpSession session = httpRequest.getSession(false);
        if (Objects.nonNull(session) && Objects.nonNull(session.getAttribute(AuthConstants.VERIFICATION_CODE))) {
            VerificationCode verificationCode = (VerificationCode) session.getAttribute(AuthConstants.VERIFICATION_CODE);
            // 2. 校验接收方
            if (!StringUtils.equals(receiver, verificationCode.getReceiver())) {
                return false;
            }

            // 3. 校验验证码
            boolean result = Instant.now(Clock.systemDefaultZone()).isBefore(verificationCode.getExpireTime()) && StringUtils.equals(code, verificationCode.getCode());
            if (result) {
                session.removeAttribute(AuthConstants.VERIFICATION_CODE);
            }
            return result;
        }
        return false;
    }

    /**
     * 校验验证结果 token
     *
     * @param token 验证结果 token
     * @return 校验结果
     */
    @Override
    public boolean verifyResultToken(String token) {
        if (StringUtils.isNotEmpty(token)) {
            return JwtUtil.verifyJwtWithHS256(token, jws256);
        }
        return false;
    }

    /**
     * 检查验证码
     *
     * @param requestDto 请求
     * @return 检查结果
     */
    @Override
    public CheckCodeResponseDto checkCode(CheckCodeRequestDto requestDto) {
        String code = requestDto.getCode();
        String username = requestDto.getUsername();
        if (verifyCode(username, code)) {
            CheckCodeResponseDto responseDto = new CheckCodeResponseDto();
            responseDto.setResultToken(JwtUtil.createJwtWithHS256(Map.of(JWTClaimNames.SUBJECT, username), jws256, checkCodeResultLive, ChronoUnit.MINUTES));
            return responseDto;
        }
        throw new BizException(MessageConstants.VERIFY_CODE_MSG_1000);
    }
}
