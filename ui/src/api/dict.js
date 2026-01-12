import apiRequest from "@/util/apiRequest";
/**
 * 获取字典列表
 *
 * @param params 请求参数
 * @returns 字典列表
 */
export function getDictList(params) {
    return apiRequest.get({
        url: "/dict/list",
        params: params,
    });
}
/**
 * 获取字典数据列表
 *
 * @param params 请求参数
 * @returns 字典数据列表
 */
export function getDictDataList(dictId, params) {
    return apiRequest.get({
        url: "/dict/".concat(dictId, "/data/list"),
        params: params,
    });
}
/**
 * 获取字典详情
 *
 * @param dictId 字典ID
 * @returns 字典详情
 */
export function getDictDetail(dictId) {
    return apiRequest.get({
        url: "/dict/".concat(dictId),
    });
}
/**
 * 更新字典信息
 *
 * @param data 字典信息
 * @returns 调用结果
 */
export function updateDict(data) {
    return apiRequest.put({
        url: "/dict",
        data: data,
    });
}
/**
 * 获取字典数据详情
 *
 * @param dictDataId 字典数据ID
 * @returns 字典数据详情
 */
export function getDictDataDetail(dictDataId) {
    return apiRequest.get({
        url: "/dict/data/".concat(dictDataId),
    });
}
/**
 * 更新字典数据信息
 *
 * @param data 字典数据信息
 * @returns 调用结果
 */
export function updateDictData(data) {
    return apiRequest.put({
        url: "/dict/data",
        data: data,
    });
}
/**
 * 创建字典数据
 *
 * @param data 字典数据信息
 * @returns 调用结果
 */
export function createDictData(data) {
    return apiRequest.post({
        url: "/dict/data",
        data: data,
    });
}
/**
 * 删除字典数据
 *
 * @param dictDataId 字典数据ID
 * @returns 调用结果
 */
export function deleteDictData(dictDataId) {
    return apiRequest.delete({
        url: "/dict/data/".concat(dictDataId),
    });
}
/**
 * 创建字典
 *
 * @param data 字典数据
 * @returns 调用结果
 */
export function createDict(data) {
    return apiRequest.post({
        url: "/dict",
        data: data,
    });
}
/**
 * 删除字典
 *
 * @param dictId 字典ID
 * @returns 调用结果
 */
export function deleteDict(dictId) {
    return apiRequest.delete({
        url: "/dict/".concat(dictId),
    });
}
/**
 * 获取启用的字典数据
 *
 * @param dictId 字典ID
 * @returns 启用的字典数据
 */
export function getEnabledDictData(dictId) {
    return apiRequest.get({
        url: "/dict/".concat(dictId, "/data/enabled"),
    });
}
/**
 * 获取可选子字典列表
 *
 * @param dictId 字典ID
 * @returns 可选子字典列表
 */
export function getSelectableChildDictList(dictId) {
    return apiRequest.get({
        url: "/dict/".concat(dictId, "/child/selectable"),
    });
}
/**
 * 添加子字典
 *
 * @param data 子字典数据
 * @returns 调用结果
 */
export function addChildDicts(data) {
    return apiRequest.put({
        url: "/dict/child",
        data: data,
    });
}
/**
 * 删除子字典
 *
 * @param data 子字典数据
 * @returns 调用结果
 */
export function removeChildDicts(data) {
    return apiRequest.delete({
        url: "/dict/child",
        data: data,
    });
}
/**
 * 获取可关联字典数据列表
 *
 * @param dictId 字典ID
 * @returns 可关联字典数据列表
 */
export function getRelatableDictDataList(dictId) {
    return apiRequest.get({
        url: "/dict/".concat(dictId, "/data/relatable"),
    });
}
