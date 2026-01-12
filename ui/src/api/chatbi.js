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
export function getDataSourceConfList(params) {
    return apiRequest.get({
        url: "/chatbi/dataSourceConf/list",
        params: params,
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
export function getModelProviderList(params) {
    return apiRequest.get({
        url: "/chatbi/modelProvider/list",
        params: params,
    });
}
/**
 * 投票回答
 *
 * @param data 入参
 * @returns 调用结果
 */
export function voteAnswer(data) {
    return noneLoadingApiRequest.post({
        url: "/chatbi/answer/vote",
        data: data,
    });
}
/**
 * 获取数据源配置详情
 *
 * @param id 数据源ID
 * @returns 数据源配置详情
 */
export function getDataSourceConfDetail(id) {
    return apiRequest.get({
        url: "/chatbi/dataSourceConf/".concat(id),
    });
}
/**
 * 同步表
 *
 * @param id 数据源ID
 * @returns 响应结果
 */
export function syncTable(id) {
    return apiRequest.post({
        url: "/chatbi/dataSourceConf/".concat(id, "/syncTable"),
    });
}
/**
 * 创建数据源配置
 *
 * @param data 创建数据源配置表单
 * @returns 响应结果
 */
export function createDataSourceConf(data) {
    return apiRequest.post({
        url: "/chatbi/dataSourceConf",
        data: data,
    });
}
/**
 * 更新数据源配置
 *
 * @param data 更新数据源配置表单
 * @returns 响应结果
 */
export function updateDataSourceConf(data) {
    return apiRequest.put({
        url: "/chatbi/dataSourceConf",
        data: data,
    });
}
/**
 * 测试数据源连接
 *
 * @param data 测试数据源连接表单
 * @returns 响应结果
 */
export function testDataSourceConn(data) {
    return apiRequest.post({
        url: "/chatbi/dataSourceConf/testConn",
        data: data,
    });
}
/**
 * 获取数据源下的表列表
 *
 * @param dataSourceId 数据源ID
 * @param params 请求参数
 * @returns 数据源下的表列表
 */
export function getTableList(dataSourceId, params) {
    return apiRequest.get({
        url: "/chatbi/dataSourceConf/".concat(dataSourceId, "/table/list"),
        params: params,
    });
}
/**
 * 批量更新表
 *
 * @param data 请求数据
 * @returns 响应结果
 */
export function batchUpdateTable(data) {
    return apiRequest.put({
        url: "/chatbi/table/batchUpdate",
        data: data,
    });
}
/**
 * 获取表字段列表
 *
 * @param tableId 表ID
 * @param params 请求参数
 * @returns 表字段列表
 */
export function getTableFieldList(tableId, params) {
    return apiRequest.get({
        url: "/chatbi/table/".concat(tableId, "/field/list"),
        params: params,
    });
}
/**
 * 批量更新表字段
 *
 * @param data 请求数据
 * @returns 响应结果
 */
export function batchUpdateTableField(data) {
    return apiRequest.put({
        url: "/chatbi/table/field/batchUpdate",
        data: data,
    });
}
/**
 * 删除数据源配置
 *
 * @param id 数据源ID
 * @returns 响应结果
 */
export function deleteDataSourceConf(id) {
    return apiRequest.delete({
        url: "/chatbi/dataSourceConf/".concat(id),
    });
}
/**
 * 获取当前用户对话记录
 *
 * @param keyword 搜索关键词
 * @returns 当前用户对话记录
 */
export function getUserChatHistory(keyword) {
    return apiRequest.get({
        url: "/chatbi/chat/history",
        params: {
            keyword: keyword,
        },
    });
}
/**
 * 获取当前用户对话消息记录
 *
 * @param id 对话ID
 * @returns 当前用户对话消息记录
 */
export function getUserChatMessageHistory(id) {
    return apiRequest.get({
        url: "/chatbi/chat/".concat(id, "/history"),
    });
}
/**
 * 删除当前用户对话记录
 *
 * @param id 对话ID
 * @returns 响应结果
 */
export function deleteChatHistory(id) {
    return apiRequest.delete({
        url: "/chatbi/chat/".concat(id),
    });
}
/**
 * 更新当历史对话标题
 *
 * @param data 更新历史对话标题表单
 * @returns 响应结果
 */
export function updateChatHistoryTitle(data) {
    return apiRequest.put({
        url: "/chatbi/chat/history",
        data: data,
    });
}
/**
 * 更新模型提供商
 *
 * @param data  更新模型提供商表单
 * @returns  响应结果
 */
export function updateModelProvider(data) {
    return apiRequest.put({
        url: "/chatbi/modelProvider",
        data: data,
    });
}
/**
 * 创建模型提供商
 *
 * @param data 创建模型提供商表单
 * @returns 响应结果
 */
export function createModelProvider(data) {
    return apiRequest.post({
        url: "/chatbi/modelProvider",
        data: data,
    });
}
/**
 * 获取模型提供商详情
 *
 * @param id 模型提供商ID
 * @returns 模型提供商详情
 */
export function getModelProviderDetail(id) {
    return apiRequest.get({
        url: "/chatbi/modelProvider/".concat(id),
    });
}
/**
 * 删除模型提供商
 *
 * @param id 模型提供商ID
 * @returns 响应结果
 */
export function deleteModelProvider(id) {
    return apiRequest.delete({
        url: "/chatbi/modelProvider/".concat(id),
    });
}
/**
 * 获取回答的 SQL
 *
 * @param id 回答ID
 * @returns 回答的 SQL
 */
export function getAnsweredSql(id) {
    return noneLoadingApiRequest.get({
        url: "/chatbi/answer/".concat(id, "/sql"),
    });
}
