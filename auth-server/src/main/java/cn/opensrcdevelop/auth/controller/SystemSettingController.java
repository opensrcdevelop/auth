package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.MailServiceConfigDto;
import cn.opensrcdevelop.auth.biz.dto.MailTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.MailTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.service.MailTemplateService;
import cn.opensrcdevelop.auth.biz.service.SystemSettingService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "API-Setting", description = "接口-系统设置")
@RestController
@RestResponse
@RequestMapping("/setting")
@RequiredArgsConstructor
public class SystemSettingController {

    private final MailTemplateService mailTemplateService;
    private final SystemSettingService systemSettingService;

    @Operation(summary = "获取邮件模版列表", description = "获取邮件模版列表")
    @GetMapping("/mailTemplate/list")
    @Authorize({ "allSystemSettingPermissions", "getMailTemplateList" })
    public List<MailTemplateResponseDto> mailTemplateList() {
        return mailTemplateService.templateList();
    }

    @Operation(summary = "获取邮件模版详情", description = "获取邮件模版详情")
    @Parameters({
            @Parameter(name = "id", description = "邮件模版ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/mailTemplate/{id}")
    @Authorize({ "allSystemSettingPermissions", "getMailTemplateDetail" })
    public MailTemplateResponseDto mailTemplateDetail(@PathVariable @NotBlank String id) {
        return mailTemplateService.detail(id);
    }

    @Operation(summary = "更新邮件模版", description = "更新邮件模版")
    @PutMapping("/mailTemplate")
    @Authorize({ "allSystemSettingPermissions", "updateMailTemplate" })
    public void updateMailTemplate(@RequestBody @Valid MailTemplateRequestDto requestDto) {
        mailTemplateService.update(requestDto);
    }


    @Operation(summary = "获取邮件服务配置", description = "获取邮件服务配置")
    @GetMapping("/mailService")
    @Authorize({ "allSystemSettingPermissions", "getMailServiceConfig" })
    public MailServiceConfigDto getMailServiceConfig() {
        return systemSettingService.getMailServerConfig();
    }

    @Operation(summary = "保存邮件服务配置", description = "保存邮件服务配置")
    @PostMapping("/mailService")
    @Authorize({ "allSystemSettingPermissions", "saveMailServiceConfig" })
    public void saveMailServiceConfig(@RequestBody @Valid MailServiceConfigDto requestDto) {
        systemSettingService.saveMailServerConfig(requestDto);
    }

    @Operation(summary = "获取邮件消息配置", description = "获取邮件消息配置")
    @GetMapping("/mailMessage")
    @Authorize({ "allSystemSettingPermissions", "getMailMessageConfig" })
    public MailMessageConfigDto getMailMessageConfig() {
        return systemSettingService.getMailMessageConfig();
    }

    @Operation(summary = "保存邮件消息配置", description = "保存邮件消息配置")
    @PostMapping("/mailMessage")
    @Authorize({ "allSystemSettingPermissions", "saveMailMessageConfig" })
    public void saveMailMessageConfig(@RequestBody @Valid MailMessageConfigDto requestDto) {
        systemSettingService.saveMailMessageConfig(requestDto);
    }
}
