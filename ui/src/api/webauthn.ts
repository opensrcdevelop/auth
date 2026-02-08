import apiRequest from "@/util/apiRequest";
import authRequest from "@/util/authRequest";

export interface WebAuthnCredentialResponse {
  id: string;
  idPrefix: string;
  deviceType: string;
  transports: string[];
  createdAt: string;
  lastUsedAt: string;
  deletable: boolean;
}

/**
 * 获取 WebAuthn 注册选项
 */
export function getWebAuthnRegisterOptions() {
  return apiRequest.post({ url: "/webauthn/register/options" });
}

/**
 * 完成 WebAuthn 注册
 * @param data WebAuthn 注册响应数据
 */
export function completeWebAuthnRegistration(data: {
  id: string;
  rawId: string;
  response: {
    clientDataJSON: string;
    attestationObject: string;
  };
  transports?: string;
}) {
  return apiRequest.post({ url: "/webauthn/register/complete", data });
}

/**
 * 获取 WebAuthn 认证选项
 */
export function getWebAuthnAuthenticateOptions() {
  return apiRequest.post({ url: "/webauthn/authenticate/options" });
}

/**
 * 完成 WebAuthn 认证（已登录用户 MFA 验证用）
 */
export function completeWebAuthnAuthentication(data: {
  id: string;
  response: string;
  clientDataJSON: string;
  signature: string;
}) {
  return apiRequest.post({ url: "/webauthn/authenticate/complete", data });
}

/**
 * 列出用户凭证
 */
export function listWebAuthnCredentials() {
  return apiRequest.get({ url: "/webauthn/credentials" });
}

/**
 * 删除凭证
 */
export function deleteWebAuthnCredential(credentialId: string) {
  return apiRequest.delete({ url: `/webauthn/credentials/${credentialId}` });
}

/**
 * Passkey 登录提交
 *
 * @param data 登录表单数据
 * @returns 登录状态
 */
export function passkeyLoginSubmit(data: any) {
  return authRequest.post({
    url: "/login/passkey",
    data,
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
    },
  });
}
