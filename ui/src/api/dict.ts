import apiRequest from "@/util/apiRequest";

/**
 * 获取字典列表
 *
 * @param params 请求参数
 * @returns 字典列表
 */
export function getDictList(params: any) {
  return apiRequest.get({
    url: "/dict/list",
    params,
  });
}

/**
 * 获取字典数据列表
 *
 * @param params 请求参数
 * @returns 字典数据列表
 */
export function getDictDataList(dictId: string, params: any) {
  return apiRequest.get({
    url: `/dict/${dictId}/data/list`,
    params,
  });
}

/**
 * 获取字典详情
 *
 * @param dictId 字典ID
 * @returns 字典详情
 */
export function getDictDetail(dictId: string) {
  return apiRequest.get({
    url: `/dict/${dictId}`,
  });
}

/**
 * 更新字典信息
 *
 * @param data 字典信息
 * @returns 调用结果
 */
export function updateDict(data: any) {
  return apiRequest.put({
    url: "/dict",
    data,
  });
}

/**
 * 获取字典数据详情
 *
 * @param dictDataId 字典数据ID
 * @returns 字典数据详情
 */
export function getDictDataDetail(dictDataId: string) {
  return apiRequest.get({
    url: `/dict/data/${dictDataId}`,
  });
}

/**
 * 更新字典数据信息
 *
 * @param data 字典数据信息
 * @returns 调用结果
 */
export function updateDictData(data: any) {
  return apiRequest.put({
    url: "/dict/data",
    data,
  });
}

/**
 * 创建字典数据
 *
 * @param data 字典数据信息
 * @returns 调用结果
 */
export function createDictData(data: any) {
  return apiRequest.post({
    url: "/dict/data",
    data,
  });
}

/**
 * 删除字典数据
 *
 * @param dictDataId 字典数据ID
 * @returns 调用结果
 */
export function deleteDictData(dictDataId: string) {
  return apiRequest.delete({
    url: `/dict/data/${dictDataId}`,
  });
}

/**
 * 创建字典
 *
 * @param data 字典数据
 * @returns 调用结果
 */
export function createDict(data: any) {
  return apiRequest.post({
    url: "/dict",
    data,
  });
}

/**
 * 删除字典
 *
 * @param dictId 字典ID
 * @returns 调用结果
 */
export function deleteDict(dictId: string) {
  return apiRequest.delete({
    url: `/dict/${dictId}`,
  });
}

/**
 * 获取启用的字典数据
 *
 * @param dictId 字典ID
 * @returns 启用的字典数据
 */
export function getEnabledDictData(dictId: string) {
  return apiRequest.get({
    url: `/dict/${dictId}/data/enabled`,
  });
}
