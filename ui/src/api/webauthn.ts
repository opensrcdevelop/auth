import apiRequest from "@/util/apiRequest";

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
  return apiRequest.post("/webauthn/register/options");
}

/**
 * 完成 WebAuthn 注册
 */
export function completeWebAuthnRegistration(data: {
  id: string;
  response: string;
  attestationObject: string;
  transports?: string;
}) {
  return apiRequest.post("/webauthn/register/complete", data);
}

/**
 * 获取 WebAuthn 认证选项（已登录用户 MFA 验证用）
 */
export function getWebAuthnAuthenticateOptions() {
  return apiRequest.post("/webauthn/authenticate/options");
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
  return apiRequest.post("/webauthn/authenticate/complete", data);
}

/**
 * 列出用户凭证
 */
export function listWebAuthnCredentials(): Promise<WebAuthnCredentialResponse[]> {
  return apiRequest.get("/webauthn/credentials");
}

/**
 * 删除凭证
 */
export function deleteWebAuthnCredential(credentialId: string) {
  return apiRequest.delete(`/webauthn/credentials/${credentialId}`);
}
