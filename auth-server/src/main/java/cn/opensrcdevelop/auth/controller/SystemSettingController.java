package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.system.mail.MailMessageConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailServiceConfigDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.dto.system.password.*;
import cn.opensrcdevelop.auth.biz.service.MailTemplateService;
import cn.opensrcdevelop.auth.biz.service.PasswordPolicyService;
import cn.opensrcdevelop.auth.biz.service.SystemSettingService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
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
    private final PasswordPolicyService passwordPolicyService;

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

    @Operation(summary = "创建密码策略", description = "创建密码策略")
    @PostMapping("/passwordPolicy")
    public void createPasswordPolicy(@RequestBody @Validated({ValidationGroups.Operation.INSERT.class })PasswordPolicyRequestDto requestDto) {
        passwordPolicyService.createPasswordPolicy(requestDto);
    }

    @Operation(summary = "获取密码策略列表", description = "获取密码策略列表")
    @GetMapping("/passwordPolicy/list")
    public List<PasswordPolicyResponseDto> passwordPolicyList() {
        return passwordPolicyService.getList();
    }

    @Operation(summary = "获取密码策略详情", description = "获取密码策略详情")
    @Parameters({
            @Parameter(name = "id", description = "密码策略ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/passwordPolicy/{id}")
    public PasswordPolicyResponseDto passwordPolicyDetail(@PathVariable @NotBlank String id ) {
        return passwordPolicyService.detail(id);
    }

    @Operation(summary = "更新密码策略优先级", description = "更新密码策略优先级")
    @PutMapping("/passwordPolicy/priority")
    public void updatePasswordPolicyPriority(@RequestBody @NotEmpty @Valid List<UpdatePasswordPolicyPriorityRequestDto> requestDtoList) {
        passwordPolicyService.updatePasswordPolicyPriority(requestDtoList);
    }

    @Operation(summary = "更新密码策略", description = "更新密码策略")
    @PutMapping("/passwordPolicy")
    public void updatePasswordPolicy(@RequestBody @Validated({ ValidationGroups.Operation.UPDATE.class }) PasswordPolicyRequestDto requestDto) {
        passwordPolicyService.updatePasswordPolicy(requestDto);
    }

    @Operation(summary = "删除密码策略", description = "删除密码策略")
    @DeleteMapping("/passwordPolicy/{id}")
    public void deletePasswordPolicy(@PathVariable @NotBlank String id) {
        passwordPolicyService.deletePasswordPolicy(id);
    }

    @Operation(summary = "检查密码强度（直接使用密码策略）", description = "检查密码强度（直接使用密码策略）")
    @PostMapping("/passwordPolicy/check")
    public CheckPasswordStrengthResponseDto checkPasswordStrength(@RequestBody @Validated CheckPasswordStrengthWithPolicyRequestDto requestDto) {
        return passwordPolicyService.checkPasswordStrength(requestDto);
    }

    @Operation(summary = "检查密码强度", description = "检查密码强度")
    @PostMapping("/passwordPolicy/checkWithoutPolicy")
    public CheckPasswordStrengthResponseDto checkPasswordStrengthWithoutPolicy(@RequestBody @Validated CheckPasswordStrengthRequestDto requestDto) {
        return passwordPolicyService.checkPasswordStrength(requestDto);
    }
}
