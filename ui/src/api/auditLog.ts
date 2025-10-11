import apiRequest from "@/util/apiRequest";

/**
 * 获取系统操作日志
 * 
 * @param params 请求参数
 * @returns 系统操作日志
 */
export function getSysOperationLogs(params: any) {
  return apiRequest.get({
    url: "/auditLog/sysOperation",
    params,
  });
}


/**
 * 获取用户操作日志
 * 
 * @param params 请求参数
 * @returns 用户操作日志
 */
export function getUserOperationLogs(params: any) {
  return apiRequest.get({
    url: "/auditLog/userOperation",
    params,
  });
}

/**
 * 获取对象变更日志
 * 
 * @param auditLogId 审计日志ID 
 * @returns 对象变更日志
 */
export function getObjChanges(auditLogId: string) {
  return apiRequest.get({
    url: `/auditLog/${auditLogId}/objChanges`,
  });
}
