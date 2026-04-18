import apiRequest from "@/util/apiRequest";

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
