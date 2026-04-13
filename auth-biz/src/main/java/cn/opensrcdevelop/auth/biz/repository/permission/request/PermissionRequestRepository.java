package cn.opensrcdevelop.auth.biz.repository.permission.request;

import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequestItem;
import cn.opensrcdevelop.common.response.PageData;
import java.util.List;

public interface PermissionRequestRepository {

    /**
     * 根据申请ID获取申请记录
     *
     * @param requestId
     *            申请ID
     * @return 申请记录
     */
    PermissionRequest getById(String requestId);

    /**
     * 根据申请人ID查询申请记录列表
     *
     * @param userId
     *            申请人ID
     * @return 申请记录列表
     */
    List<PermissionRequest> findByUserId(String userId);

    /**
     * 根据状态查询申请记录列表（状态在申请明细上，需从 item 反查）
     *
     * @param status
     *            申请状态
     * @return 申请记录列表
     */
    List<PermissionRequest> findByStatus(String status);

    /**
     * 分页查询所有申请记录
     *
     * @param page
     *            页码
     * @param pageSize
     *            每页数量
     * @return 分页后的申请记录列表
     */
    PageData<PermissionRequest> findByTenantId(int page, int pageSize);

    /**
     * 根据状态分页查询申请记录（状态在申请明细上，需从 item 反查）
     *
     * @param status
     *            申请状态
     * @param page
     *            页码
     * @param pageSize
     *            每页数量
     * @return 分页后的申请记录列表
     */
    PageData<PermissionRequest> findByStatus(String status, int page, int pageSize);

    /**
     * 检查用户是否对指定权限列表中的任意权限存在 PENDING 或 AUTO_APPROVED 的申请记录 用于重复申请检测（per D-05）
     *
     * @param userId
     *            申请人ID
     * @param permissionIds
     *            待检查的权限ID列表
     * @return true 表示存在重复申请
     */
    boolean hasActivePendingRequest(String userId, java.util.List<String> permissionIds);

    /**
     * 根据申请人ID分页查询申请记录
     *
     * @param userId
     *            申请人ID
     * @param page
     *            页码
     * @param pageSize
     *            每页数量
     * @return 分页后的申请记录列表
     */
    PageData<PermissionRequest> findByUserIdPaged(String userId, int page, int pageSize);

    /**
     * 根据状态列表分页查询申请记录（多租户隔离）
     *
     * @param statuses
     *            申请状态列表
     * @param page
     *            页码
     * @param pageSize
     *            每页数量
     * @return 分页后的申请记录列表
     */
    PageData<PermissionRequest> findByStatuses(List<String> statuses, int page, int pageSize);

    /**
     * 根据申请ID列表批量查询申请明细
     *
     * @param requestIds
     *            申请ID列表
     * @return 申请明细列表
     */
    List<PermissionRequestItem> findByRequestIds(List<String> requestIds);
}
