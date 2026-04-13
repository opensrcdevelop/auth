package cn.opensrcdevelop.auth.biz.service.permission.request;

import cn.opensrcdevelop.auth.biz.dto.permission.request.ApproveRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.RejectRequestDto;
import cn.opensrcdevelop.common.response.PageData;

/**
 * 管理员权限申请服务
 */
public interface PermissionRequestAdminService {

    /**
     * 获取待审批权限申请列表（PENDING + AUTO_APPROVED）
     *
     * @param page
     *            页码
     * @param size
     *            每页数量
     * @return 分页的申请记录列表
     */
    PageData<PermissionRequestListItemDto> listPendingRequests(int page, int size);

    /**
     * 获取所有权限申请列表（支持按状态筛选）
     *
     * @param page
     *            页码
     * @param size
     *            每页数量
     * @param status
     *            申请状态（可选，不传则返回全部）
     * @return 分页的申请记录列表
     */
    PageData<PermissionRequestListItemDto> listAllRequests(int page, int size, String status);

    /**
     * 批准权限申请
     *
     * @param requestId
     *            申请ID
     * @param dto
     *            批准请求（含限制条件）
     */
    void approveRequest(String requestId, ApproveRequestDto dto);

    /**
     * 拒绝权限申请
     *
     * @param requestId
     *            申请ID
     * @param dto
     *            拒绝请求（含拒绝理由）
     */
    void rejectRequest(String requestId, RejectRequestDto dto);
}
