import apiRequest from "@/util/apiRequest";
import noneLoadingApiRequest from "@/util/noneApiLoadingRequest";

/**
 * 获取已启用的数据源配置
 *
 * @returns 已启用的数据源配置
 */
export function getEnabledDataSourceConf() {
  return apiRequest.get({
    url: "/chatbi/dataSourceConf/enabled",
  });
}

/**
 * 获取数据源配置列表
 *
 * @param params 请求参数
 * @returns 数据源配置列表
 */
export function getDataSourceConfList(params: any) {
  return apiRequest.get({
    url: "/chatbi/dataSourceConf/list",
    params,
  });
}

/**
 * 获取已启用的模型提供商列表
 *
 * @returns 已启用的模型提供商列表
 */
export function getEnabledModelProvider() {
  return apiRequest.get({
    url: "/chatbi/modelProvider/enabled",
  });
}

/**
 * 获取模型提供商列表
 *
 * @param params 请求参数
 * @returns 模型提供商列表
 */
export function getModelProviderList(params: any) {
  return apiRequest.get({
    url: "/chatbi/modelProvider/list",
    params,
  });
}

/**
 * 投票回答
 *
 * @param data 入参
 * @returns 调用结果
 */
export function voteAnswer(data: any) {
  return noneLoadingApiRequest.post({
    url: "/chatbi/answer/vote",
    data,
  });
}

/**
 * 获取数据源配置详情
 *
 * @param id 数据源ID
 * @returns 数据源配置详情
 */
export function getDataSourceConfDetail(id: string) {
  return apiRequest.get({
    url: `/chatbi/dataSourceConf/${id}`,
  });
}

/**
 * 同步表
 *
 * @param id 数据源ID
 * @returns 响应结果
 */
export function syncTable(id: string) {
  return apiRequest.post({
    url: `/chatbi/dataSourceConf/${id}/syncTable`,
  });
}

/**
 * 创建数据源配置
 *
 * @param data 创建数据源配置表单
 * @returns 响应结果
 */
export function createDataSourceConf(data: any) {
  return apiRequest.post({
    url: "/chatbi/dataSourceConf",
    data,
  });
}

/**
 * 更新数据源配置
 *
 * @param data 更新数据源配置表单
 * @returns 响应结果
 */
export function updateDataSourceConf(data: any) {
  return apiRequest.put({
    url: "/chatbi/dataSourceConf",
    data,
  });
}

/**
 * 测试数据源连接
 *
 * @param data 测试数据源连接表单
 * @returns 响应结果
 */
export function testDataSourceConn(data: any) {
  return apiRequest.post({
    url: "/chatbi/dataSourceConf/testConn",
    data,
  });
}

/**
 * 获取数据源下的表列表
 *
 * @param dataSourceId 数据源ID
 * @param params 请求参数
 * @returns 数据源下的表列表
 */
export function getTableList(dataSourceId: string, params: any) {
  return apiRequest.get({
    url: `/chatbi/dataSourceConf/${dataSourceId}/table/list`,
    params,
  });
}

/**
 * 批量更新表
 *
 * @param data 请求数据
 * @returns 响应结果
 */
export function batchUpdateTable(data: any) {
  return apiRequest.put({
    url: "/chatbi/table/batchUpdate",
    data,
  });
}

/**
 * 获取表字段列表
 *
 * @param tableId 表ID
 * @param params 请求参数
 * @returns 表字段列表
 */
export function getTableFieldList(tableId: string, params: any) {
  return apiRequest.get({
    url: `/chatbi/table/${tableId}/field/list`,
    params,
  });
}

/**
 * 批量更新表字段
 *
 * @param data 请求数据
 * @returns 响应结果
 */
export function batchUpdateTableField(data: any) {
  return apiRequest.put({
    url: "/chatbi/table/field/batchUpdate",
    data,
  });
}

/**
 * 删除数据源配置
 *
 * @param id 数据源ID
 * @returns 响应结果
 */
export function deleteDataSourceConf(id: string) {
  return apiRequest.delete({
    url: `/chatbi/dataSourceConf/${id}`,
  });
}

/**
 * 获取当前用户对话记录
 *
 * @param keyword 搜索关键词
 * @returns 当前用户对话记录
 */
export function getUserChatHistory(keyword: string) {
  return apiRequest.get({
    url: "/chatbi/chat/history",
    params: {
      keyword,
    },
  });
}

/**
 * 获取当前用户对话消息记录
 *
 * @param id 对话ID
 * @returns 当前用户对话消息记录
 */
export function getUserChatMessageHistory(id: string) {
  return apiRequest.get({
    url: `/chatbi/chat/${id}/history`,
  });
}

/**
 * 删除当前用户对话记录
 *
 * @param id 对话ID
 * @returns 响应结果
 */
export function deleteChatHistory(id: string) {
  return apiRequest.delete({
    url: `/chatbi/chat/${id}`,
  });
}

/**
 * 更新当历史对话标题
 *
 * @param data 更新历史对话标题表单
 * @returns 响应结果
 */
export function updateChatHistoryTitle(data: any) {
  return apiRequest.put({
    url: "/chatbi/chat/history",
    data,
  });
}

/**
 * 更新模型提供商
 *
 * @param data  更新模型提供商表单
 * @returns  响应结果
 */
export function updateModelProvider(data: any) {
  return apiRequest.put({
    url: "/chatbi/modelProvider",
    data,
  });
}

/**
 * 创建模型提供商
 *
 * @param data 创建模型提供商表单
 * @returns 响应结果
 */
export function createModelProvider(data: any) {
  return apiRequest.post({
    url: "/chatbi/modelProvider",
    data,
  });
}

/**
 * 获取模型提供商详情
 *
 * @param id 模型提供商ID
 * @returns 模型提供商详情
 */
export function getModelProviderDetail(id: string) {
  return apiRequest.get({
    url: `/chatbi/modelProvider/${id}`,
  });
}
