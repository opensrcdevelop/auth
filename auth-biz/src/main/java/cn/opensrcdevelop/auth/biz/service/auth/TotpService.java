package cn.opensrcdevelop.auth.biz.service.auth;

import cn.opensrcdevelop.auth.biz.dto.auth.TotpCodeCheckRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.TotpCodeCheckResponseDto;
import jakarta.servlet.http.HttpServletRequest;

public interface TotpService {

    TotpCodeCheckResponseDto check(TotpCodeCheckRequestDto requestDto, HttpServletRequest request);
}
