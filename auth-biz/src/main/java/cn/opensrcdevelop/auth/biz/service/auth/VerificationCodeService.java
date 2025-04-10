package cn.opensrcdevelop.auth.biz.service.auth;

import cn.opensrcdevelop.auth.biz.dto.user.CheckCodeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.CheckCodeResponseDto;

import java.time.temporal.ChronoUnit;

public interface VerificationCodeService {

    String setCode(String receiver, long liveTime, ChronoUnit timeUnit);

    boolean verifyCode(String receiver, String code);

    boolean verifyResultToken(String token);

    CheckCodeResponseDto checkCode(CheckCodeRequestDto requestDto);
}
