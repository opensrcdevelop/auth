import apiRequest from "@/util/apiRequest";

/**
 * 获取当前用户已有权限
 * 对应 D-05: 复用 Phase 2 的 GET /api/v1/permissions/me API
 */
export function getMyPermissions() {
  return apiRequest.get({
    url: "/api/v1/permissions/me",
  });
}

/**
 * 获取可申请权限树
 * 对应 D-06: 展示可申请权限树（ResourceGroup → Resource → Permission）
 */
export function getAvailablePermissionTree() {
  return apiRequest.get({
    url: "/api/v1/permissions/available-tree",
  });
}

/**
 * 提交权限申请
 * 对应 D-10: 提交按钮调用 POST /user-center/permissions/requests
 */
export function submitPermissionRequest(data: {
  permissionIds: string[];
  reason: string;
}) {
  return apiRequest.post({
    url: "/user-center/permissions/requests",
    data,
  });
}

/**
 * 获取申请记录列表
 * 对应 D-12: 调用 GET /user-center/permissions/requests 获取列表
 */
export function getRequestList(params: { page: number; size: number }) {
  return apiRequest.get({
    url: "/user-center/permissions/requests",
    params,
  });
}

/**
 * 获取申请详情
 * 对应 D-13: 调用 GET /user-center/permissions/requests/{id} 查看详情
 */
export function getRequestDetail(id: string) {
  return apiRequest.get({
    url: `/user-center/permissions/requests/${id}`,
  });
}
