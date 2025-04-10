package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.user.CheckCodeRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.CheckCodeResponseDto;
import cn.opensrcdevelop.auth.biz.service.auth.VerificationCodeService;
import cn.opensrcdevelop.auth.biz.service.system.mail.MailService;
import cn.opensrcdevelop.common.annoation.RestResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Email;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@Tag(name = "API-VerificationCode", description = "接口-验证码")
@RestController
@RestResponse
@RequestMapping("/code")
@RequiredArgsConstructor
public class VerificationCodeController {

    private final MailService mailService;
    private final VerificationCodeService verificationCodeService;

    @Operation(summary = "发送邮箱验证码", description = "发送邮箱验证码")
    @PostMapping("/email/{to}")
    public void sendMailCode(@PathVariable @Email String to) {
        mailService.sendMailCode(to);
    }


    @Operation(summary = "检查验证码", description = "检查验证码")
    @PostMapping("/check")
    public CheckCodeResponseDto checkCode(@RequestBody @Valid CheckCodeRequestDto requestDto) {
        return verificationCodeService.checkCode(requestDto);
    }

    @Operation(summary = "发送绑定邮箱验证码", description = "发送绑定邮箱验证码")
    @PostMapping("/email/bind/{to}")
    public void sendBindEmailCode(@PathVariable @Email String to) {
        mailService.sendBindEmailCode(to);
    }
}
