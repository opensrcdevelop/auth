package cn.opensrcdevelop.auth.biz.service.permission.request;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;

/**
 * 权限申请提交服务
 */
public interface PermissionRequestService {

    /**
     * 提交权限申请 支持批量申请多个权限，共用一个申请理由（per D-01） 开启自动批准的权限即时写入 t_authorize（per D-02,
     * D-03） 存在重复 PENDING 或 AUTO_APPROVED 申请时拒绝（per D-05）
     *
     * @param dto
     *            申请请求（permissionIds + reason）
     * @return 申请结果（requestId + 各状态数量统计）
     */
    PermissionRequestResponseDto submitRequest(PermissionRequestCreateDto dto);
}
