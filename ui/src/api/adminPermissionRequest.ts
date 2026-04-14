import apiRequest from "@/util/apiRequest";

/**
 * 获取待审批权限申请列表
 * 对应 D-05: GET /admin/permissions/requests/pending
 * 返回 PENDING 和 AUTO_APPROVED 状态的申请
 */
export function getPendingList(params: { page: number; size: number }) {
  return apiRequest.get({
    url: "/admin/permissions/requests/pending",
    params,
  });
}

/**
 * 获取所有权限申请列表
 * 对应 D-05: GET /admin/permissions/requests/all?status=
 * 支持按状态筛选，不传 status 返回全部
 */
export function getAllList(params: {
  page: number;
  size: number;
  status?: 'PENDING' | 'APPROVED' | 'REJECTED' | 'AUTO_APPROVED';
}) {
  return apiRequest.get({
    url: "/admin/permissions/requests/all",
    params,
  });
}

/**
 * 批准权限申请
 * 对应 D-17: POST /admin/permissions/requests/{id}/approve
 */
export function approveRequest(requestId: string, data: {
  itemIds?: string[];
  expressionIds?: string[];
  priority?: number;
}) {
  return apiRequest.post({
    url: `/admin/permissions/requests/${requestId}/approve`,
    data,
  });
}

/**
 * 拒绝权限申请
 * 对应 D-21: POST /admin/permissions/requests/{id}/reject
 */
export function rejectRequest(requestId: string, data: {
  itemIds?: string[];
  rejectReason: string;
}) {
  return apiRequest.post({
    url: `/admin/permissions/requests/${requestId}/reject`,
    data,
  });
}

/**
 * 获取权限申请详情
 * 对应 Phase 6: GET /admin/permissions/requests/{id}
 */
export function getRequestDetail(requestId: string) {
  return apiRequest.get({
    url: `/admin/permissions/requests/${requestId}`,
  });
}
