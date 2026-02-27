import apiRequest from "@/util/apiRequest";
/**
 * 获取 OIDC 所有 Scope
 *
 * @returns OIDC 所有 Scope
 */
export function getOidcScopes() {
    return apiRequest.get({
        url: "/oidc/scope/list",
    });
}
/**
 * 获取 OIDC 所有 Claim
 *
 * @returns OIDC 所有 Claim
 */
export function getOidcClaims() {
    return apiRequest.get({
        url: "/oidc/claim/list",
    });
}
/**
 * 更新 OIDC scope
 *
 * @param data OIDC scope
 * @returns 调用结果
 */
export function updateOidcScope(data) {
    return apiRequest.put({
        url: "/oidc/scope",
        data: data,
    });
}
/**
 * 创建 OIDC scope
 *
 * @param data OIDC scope
 * @returns 调用结果
 */
export function createOidcScope(data) {
    return apiRequest.post({
        url: "/oidc/scope",
        data: data,
    });
}
/**
 * 删除 OIDC Scope
 *
 * @param id Scope ID
 * @returns 调用结果
 */
export function deleteOidcScope(id) {
    return apiRequest.delete({
        url: "/oidc/scope/".concat(id),
    });
}
/**
 * 更新 OIDC Claim
 *
 * @param data OIDC Claim
 * @returns 调用结果
 */
export function updateOidcClaim(data) {
    return apiRequest.put({
        url: "/oidc/claim",
        data: data,
    });
}
/**
 * 创建 OIDC Claim
 *
 * @param data OIDC Claim
 * @returns 调用结果
 */
export function createOidcClaim(data) {
    return apiRequest.post({
        url: "/oidc/claim",
        data: data,
    });
}
/**
 * 删除 OIDC Claim
 *
 * @param id Claim ID
 * @returns 调用结果
 */
export function deleteOidcClaim(id) {
    return apiRequest.delete({
        url: "/oidc/claim/".concat(id),
    });
}
