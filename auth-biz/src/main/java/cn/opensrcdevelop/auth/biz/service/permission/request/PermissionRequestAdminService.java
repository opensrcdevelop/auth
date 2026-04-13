package cn.opensrcdevelop.auth.biz.service.permission.request;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
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
}
