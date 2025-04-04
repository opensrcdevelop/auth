package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.auth.TotpCodeCheckRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.TotpCodeCheckResponseDto;
import jakarta.servlet.http.HttpServletRequest;

public interface TotpService {

    public TotpCodeCheckResponseDto check(TotpCodeCheckRequestDto requestDto, HttpServletRequest request);
}
