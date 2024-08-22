package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.CheckCodeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.CheckCodeResponseDto;

import java.time.temporal.ChronoUnit;

public interface VerificationCodeService {

    String setCode(long liveTime, ChronoUnit timeUnit);

    boolean verifyCode(String code);

    boolean verifyResultToken(String token);

    CheckCodeResponseDto checkCode(CheckCodeRequestDto requestDto);
}
