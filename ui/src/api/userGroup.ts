import apiRequest from "@/util/apiRequest";

/**
 * 获取用户组列表
 *
 * @param params 请求参数
 * @returns 用户组列表
 */
export function getUserGroupList(params: any) {
  return apiRequest.get({
    url: "/userGroup/list",
    params,
  });
}

/**
 * 删除用户组映射
 *
 * @param data 用户组映射
 * @returns 调用结果
 */
export function removeUserGroupMapping(data: any) {
  return apiRequest.delete({
    url: "/userGroup/mapping",
    data,
  });
}

/**
 * 添加用户组映射
 *
 * @param data 用户组映射
 * @returns 调用结果
 */
export function addUserGroupMapping(data: any) {
  return apiRequest.post({
    url: "/userGroup/mapping",
    data,
  });
}

/**
 * 获取用户组详情
 *
 * @param id 用户组ID
 * @returns 用户组详情
 */
export function getUserGroupDetail(id: string) {
  return apiRequest.get({
    url: `/userGroup/${id}`,
  });
}

/**
 * 获取组内用户
 *
 * @param id 用户组 ID
 * @param params 请求参数
 * @returns 组内用户
 */
export function getGroupUsers(id: string, params: any) {
  return apiRequest.get({
    url: `/userGroup/${id}/users`,
    params,
  });
}

/**
 * 更新用户组信息
 *
 * @param data 用户组信息
 * @returns 调用结果
 */
export function updateUserGroup(data: any) {
  return apiRequest.put({
    url: "/userGroup",
    data,
  });
}

/**
 * 创建用户组
 *
 * @param data 创建用户组表单
 * @returns 调用结果
 */
export function createUserGroup(data: any) {
  return apiRequest.post({
    url: "/userGroup",
    data,
  });
}

/**
 * 删除用户组
 *
 * @param id 用户组 ID
 * @returns 调用结果
 */
export function deleteUserGroup(id: string) {
  return apiRequest.delete({
    url: `/userGroup/${id}`,
  });
}

/**
 * 获取用户组权限
 *
 * @param id 用户组 ID
 * @param params 请求参数
 * @returns 用户组权限
 */
export function getUserGroupPermissions(id: string, params: any) {
  return apiRequest.get({
    url: `/userGroup/${id}/permissions`,
    params,
  });
}
