package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.TotpCodeCheckRequestDto;
import cn.opensrcdevelop.auth.biz.dto.TotpCodeCheckResponseDto;
import cn.opensrcdevelop.auth.biz.service.TotpService;
import cn.opensrcdevelop.common.annoation.RestResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "API-Totp", description = "接口-Totp")
@RestController
@RestResponse
@RequestMapping("/totp")
@RequiredArgsConstructor
public class TotpController {

    private final TotpService totpService;

    @Operation(summary = "检验一次性密码", description = "校验一次性密码")
    @PostMapping("/check")
    public TotpCodeCheckResponseDto checkCode(@RequestBody @Valid TotpCodeCheckRequestDto requestDto, HttpServletRequest request) {
        return totpService.check(requestDto, request);
    }
}
