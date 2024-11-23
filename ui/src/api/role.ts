import apiRequest from "@/util/apiRequest";

/**
 * 删除用户角色映射
 *
 * @param data 用户角色映射
 * @returns 调用结果
 */
export function removeRoleMapping(data: any) {
  return apiRequest.delete({
    url: "/role/mapping",
    data,
  });
}

/**
 * 获取角色列表
 *
 * @param params 请求参数
 * @returns 角色列表
 */
export function getRoleList(params: any) {
  return apiRequest.get({
    url: "/role/list",
    params,
  });
}

/**
 * 添加用户角色映射
 *
 * @param data 用户角色映射
 * @returns 调用结果
 */
export function addRoleMapping(data: any) {
  return apiRequest.post({
    url: "/role/mapping",
    data,
  });
}

/**
 * 获取角色详情
 *
 * @param id 角色ID
 * @returns 角色详情
 */
export function getRoleDetail(id: string) {
  return apiRequest.get({
    url: `/role/${id}`,
  });
}

/**
 * 更新角色信息
 *
 * @param data 角色信息
 * @returns 调用结果
 */
export function updateRole(data: any) {
  return apiRequest.put({
    url: "/role",
    data,
  });
}

/**
 * 获取角色主体
 *
 * @param id 角色ID
 * @param params 请求参数
 * @returns 角色主体
 */
export function getRolePrincipals(id: string, params: any) {
  return apiRequest.get({
    url: `/role/${id}/principals`,
    params,
  });
}

/**
 * 创建角色
 *
 * @param data 角色信息表单
 * @returns 调用结果
 */
export function createRole(data: any) {
  return apiRequest.post({
    url: "/role",
    data,
  });
}

/**
 * 删除角色
 *
 * @param id 角色ID
 * @returns 调用结果
 */
export function deleteRole(id: string) {
  return apiRequest.delete({
    url: `/role/${id}`,
  });
}

/**
 * 获取角色权限
 *
 * @param id 角色 ID
 * @param params 请求参数
 * @returns 角色权限
 */
export function getRolePermissions(id: string, params: any) {
  return apiRequest.get({
    url: `/role/${id}/permissions`,
    params,
  });
}
