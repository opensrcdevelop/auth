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
export function updateOidcScope(data: any) {
  return apiRequest.put({
    url: "/oidc/scope",
    data,
  });
}

/**
 * 创建 OIDC scope
 *
 * @param data OIDC scope
 * @returns 调用结果
 */
export function createOidcScope(data: any) {
  return apiRequest.post({
    url: "/oidc/scope",
    data,
  });
}

/**
 * 删除 OIDC Scope
 *
 * @param id Scope ID
 * @returns 调用结果
 */
export function deleteOidcScope(id: string) {
  return apiRequest.delete({
    url: `/oidc/scope/${id}`,
  });
}

/**
 * 更新 OIDC Claim
 *
 * @param data OIDC Claim
 * @returns 调用结果
 */
export function updateOidcClaim(data: any) {
  return apiRequest.put({
    url: "/oidc/claim",
    data,
  });
}

/**
 * 创建 OIDC Claim
 *
 * @param data OIDC Claim
 * @returns 调用结果
 */
export function createOidcClaim(data: any) {
  return apiRequest.post({
    url: "/oidc/claim",
    data,
  });
}

/**
 * 删除 OIDC Claim
 *
 * @param id Claim ID
 * @returns 调用结果
 */
export function deleteOidcClaim(id: string) {
  return apiRequest.delete({
    url: `/oidc/claim/${id}`,
  });
}
