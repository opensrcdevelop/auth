import apiRequest from "@/util/apiRequest";

/**
 * 获取数据源配置列表
 * 
 * @param params 请求参数
 * @returns 数据源配置列表
 */
export function getDataSourceConfList(params: any) {
    return apiRequest.get({
        url: '/chatbi/dataSourceConf/list',
        params
    })
}

/**
 * 获取模型提供商列表
 * 
 * @param params 请求参数
 * @returns 模型提供商列表
 */
export function getModelProviderList(params: any) {
    return apiRequest.get({
        url: '/chatbi/modelProvider/list',
        params
    })
}