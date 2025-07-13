import apiRequest from "@/util/apiRequest";

/**
 * 取消授权
 *
 * @param permissionId 权限ID
 * @param principalId 用户 / 用户组 / 角色ID
 * @returns 调用结果
 */
export function cancelAuthorization(permissionId: string, principalId: string) {
  return apiRequest.delete({
    url: `/permission/authorize/${permissionId}/${principalId}`,
  });
}

/**
 * 获取权限详情
 *
 * @param id 权限ID
 * @params 请求参数
 * @returns 权限详情
 */
export function getPermissionDetail(id: string, params) {
  return apiRequest.get({
    url: `/permission/${id}`,
    params,
  });
}

/**
 * 创建权限
 *
 * @param data 创建权限表单
 * @returns 调用结果
 */
export function createPermission(data: any) {
  return apiRequest.post({
    url: "/permission",
    data,
  });
}

/**
 * 删除权限
 *
 * @param id 权限ID
 * @returns 调用结果
 */
export function deletePermission(id: string) {
  return apiRequest.delete({
    url: `/permission/${id}`,
  });
}

/**
 * 更新权限
 *
 * @param data 更新权限表单
 * @returns 调用结果
 */
export function updatePermission(data: any) {
  return apiRequest.put({
    url: "/permission",
    data,
  });
}

/**
 * 获取权限表达式列表
 *
 * @param params 请求参数
 */
export function getPermissionExpList(params: any) {
  return apiRequest.get({
    url: "/permission/exp/list",
    params,
  });
}

/**
 * 获取权限表达式详情
 *
 * @param id 权限表达式ID
 * @returns 权限表达式详情
 */
export function getPermissionExpDetail(id: string) {
  return apiRequest.get({
    url: `/permission/exp/${id}`,
  });
}

/**
 * 获取权限表达式关联的权限列表
 * 
 * @param id 权限表达式ID
 * @returns 权限表达式关联的权限列表
 */
export function getPermissionExpPermissions(id: string) {
  return apiRequest.get({
    url: `/permission/exp/${id}/permissions`,
  });
}

/**
 * 删除授权条件
 *
 * @param data 请求
 * @returns 响应结果
 */
export function removeAuthorizeCondition(data: any) {
  return apiRequest.delete({
    url: "/permission/authorize/cond",
    data,
  });
}

/**
 * 添加授权条件
 *
 * @param data 请求
 * @returns 响应结果
 */
export function addAuthorizeCondition(data: any) {
  return apiRequest.post({
    url: "/permission/authorize/cond",
    data,
  });
}

/**
 * 更新权限表达式
 *
 * @param data 更新权限表达式表单
 * @returns 调用结果
 */
export function updatePermissionExp(data: any) {
  return apiRequest.put({
    url: "/permission/exp",
    data,
  });
}

/**
 * 删除权限表达式
 *
 * @param id 权限表达式ID
 * @returns 调用结果
 */
export function deletePermissionExp(id: string) {
  return apiRequest.delete({
    url: `/permission/exp/${id}`,
  });
}

/**
 * 创建权限表达式
 *
 * @param data 创建权限表达式表单
 * @returns 调用结果
 */
export function createPermissionExp(data: any) {
  return apiRequest.post({
    url: "/permission/exp",
    data,
  });
}

/**
 * 授权
 *
 * @param data 授权表单
 * @returns 调用结果
 */
export function authorize(data: any) {
  return apiRequest.post({
    url: "/permission/authorize",
    data,
  });
}

/**
 * 调试权限表达式
 * 
 * @param data 调试权限表达式表单
 * @returns 调用结果
 */
export function debugPermissionExp(data: any) {
  return apiRequest.post({
    url: "/permission/exp/debug",
    data,
  })
}

/**
 * 更新授权优先级
 * 
 * @param id 授权ID
 * @param priority 优先级
 * @returns 调用结果
 */
export function updateAuthorizePriority(id: String, priority: Number){
  return apiRequest.put({
    url: `/permission/authorize/${id}/${priority}`
  })
}

/**
 * 创建权限表达式模板
 * 
 * @param data 创建权限表达式模板表单
 * @returns 调用结果
 */
export function createPermissionExpTemplate(data: any) {
  return apiRequest.post({
    url: "/permission/exp/template",
    data,
  });
}

/**
 * 获取权限表达式模板列表
 * 
 * @param params 请求参数
 * @returns 权限表达式模板列表
 */
export function getPermissionExpTemplateList(params: any) {
  return apiRequest.get({
    url: "/permission/exp/template/list",
    params,
  });
}

/**
 * 获取权限表达式模板详情
 * 
 * @param id 权限表达式模板ID
 * @returns 权限表达式模板详情
 */
export function getPermissionExpTemplateDetail(id: string) {
  return apiRequest.get({
    url: `/permission/exp/template/${id}`,
  });
}

/**
 * 更新权限表达式模板
 * 
 * @param data 更新权限表达式模板表单
 * @returns 调用结果
 */
export function updatePermissionExpTemplate(data: any) {
  return apiRequest.put({
    url: "/permission/exp/template",
    data,
  });
}

/**
 * 删除权限表达式模板
 * 
 * @param id 模板ID
 * @returns 调用结果
 */
export function deletePermissionExpTemplate(id: string) {
  return apiRequest.delete({
    url: `/permission/exp/template/${id}`,
  });
}

/**
 * 获取权限表达式模板关联的权限表达式列表
 * 
 * @param id 模版ID
 * @returns 权限表达式模板关联的权限表达式列表
 */
export function getPermissionExpTemplateExpList(id: string) {
  return apiRequest.get({
    url: `/permission/exp/template/${id}/exps`,
  })
}

/**
 * 获取权限表达式模板参数配置
 * 
 * @param id 模板ID
 * @returns 模板参数配置
 */
export function getPremissionExpTemplateParamConfigs(id: string) {
  return apiRequest.get({
    url: `/permission/exp/template/${id}/params`,
  })
}
