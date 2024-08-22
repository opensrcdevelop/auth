package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.TotpCodeCheckRequestDto;
import cn.opensrcdevelop.auth.biz.dto.TotpCodeCheckResponseDto;
import jakarta.servlet.http.HttpServletRequest;

public interface TotpService {

    public TotpCodeCheckResponseDto check(TotpCodeCheckRequestDto requestDto, HttpServletRequest request);
}
