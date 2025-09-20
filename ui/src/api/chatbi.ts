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
 * 投票图表
 *
 * @param data 入参
 * @returns 调用结果
 */
export function voteChart(data: any) {
  return noneLoadingApiRequest.post({
    url: "/chatbi/chart/vote",
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
