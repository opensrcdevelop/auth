import apiRequest from "@/util/apiRequest";
import noneLoadingApiRequest from "@/util/noneApiLoadingRequest";

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
