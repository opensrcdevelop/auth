package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.audit.AuditLogResponseDto;
import cn.opensrcdevelop.auth.biz.dto.audit.ObjChangeLogResponseDto;
import cn.opensrcdevelop.auth.biz.service.audit.AuditLogService;
import cn.opensrcdevelop.auth.biz.service.audit.ObjChangeLogService;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.response.PageData;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotBlank;
import java.time.LocalDateTime;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

@Tag(name = "API-AuditLog", description = "接口-审计日志")
@RestController
@RestResponse
@RequestMapping("/auditLog")
@RequiredArgsConstructor
public class AuditLogController {

    private final AuditLogService auditLogService;
    private final ObjChangeLogService objChangeLogService;

    @Operation(summary = "获取用户操作日志", description = "获取用户操作日志")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户ID / 用户名 / IP 检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "type", description = "操作类型", in = ParameterIn.QUERY),
            @Parameter(name = "start", description = "开始时间", in = ParameterIn.QUERY),
            @Parameter(name = "end", description = "结束时间", in = ParameterIn.QUERY)
    })
    @GetMapping("/userOperation")
    @Authorize({"allAuditLogPermissions", "listUserOperationLogs"})
    public PageData<AuditLogResponseDto> userOperationLogs(@RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size,
            @RequestParam(required = false) String keyword,
            @RequestParam(required = false) Integer type,
            @RequestParam(required = false) @DateTimeFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS) LocalDateTime start,
            @RequestParam(required = false) @DateTimeFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS) LocalDateTime end) {
        return auditLogService.getUserOperationLogs(page, size, keyword, type, start, end);
    }

    @Operation(summary = "获取系统操作日志", description = "获取系统操作日志")
    @Parameters({
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "keyword", description = "用户ID / 用户名 / IP 检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "type", description = "操作类型", in = ParameterIn.QUERY),
            @Parameter(name = "resourceId", description = "资源ID", in = ParameterIn.QUERY),
            @Parameter(name = "start", description = "开始时间", in = ParameterIn.QUERY),
            @Parameter(name = "end", description = "结束时间", in = ParameterIn.QUERY)
    })
    @GetMapping("/sysOperation")
    @Authorize({"allAuditLogPermissions", "listSysOperationLogs"})
    public PageData<AuditLogResponseDto> sysOperationLogs(@RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "15") int size,
            @RequestParam(required = false) String keyword,
            @RequestParam(required = false) Integer type,
            @RequestParam(required = false) String resourceId,
            @RequestParam(required = false) @DateTimeFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS) LocalDateTime start,
            @RequestParam(required = false) @DateTimeFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS) LocalDateTime end) {
        return auditLogService.getSysOperationLogs(page, size, keyword, type, resourceId, start, end);
    }

    @Operation(summary = "获取对象变更日志", description = "获取对象变更日志")
    @Parameters({
            @Parameter(name = "id", description = "审计日志ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/{id}/objChanges")
    @Authorize({"allAuditLogPermissions", "listObjChangeLogs"})
    public ObjChangeLogResponseDto objChangeLog(@PathVariable @NotBlank String id) {
        return objChangeLogService.getObjChangeLog(id);
    }
}
