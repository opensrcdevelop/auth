import { startAuthentication, startRegistration } from "@simplewebauthn/browser";

/**
 * WebAuthn/Passkey 工具类
 */
const webauthn = {
  /**
   * 检测浏览器是否支持 WebAuthn
   */
  isSupported(): boolean {
    return !!(
      window.PublicKeyCredential &&
      typeof window.PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable === "function"
    );
  },

  /**
   * 检测是否支持平台认证器（如 Touch ID, Windows Hello）
   */
  async isPlatformAuthenticatorAvailable(): Promise<boolean> {
    if (!window.PublicKeyCredential) {
      return false;
    }
    try {
      return await window.PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable();
    } catch {
      return false;
    }
  },

  /**
   * 开始注册流程
   *
   * @param options 注册选项
   * @returns 认证响应
   */
  async startRegistration(options: any): Promise<{
    id: string;
    response: {
      clientDataJSON: string;
      attestationObject: string;
      transports?: string[];
    };
  }> {
    try {
      const credential = await startRegistration(options);
      return {
        id: credential.id,
        response: {
          clientDataJSON: credential.response.clientDataJSON,
          attestationObject: credential.response.attestationObject,
          transports: (credential.response as any).transports,
        },
      };
    } catch (error) {
      console.error("WebAuthn 注册失败:", error);
      throw error;
    }
  },

  /**
   * 开始认证流程
   *
   * @param options 认证选项
   * @returns 认证响应
   */
  async startAuthentication(options: any): Promise<{
    id: string;
    response: {
      clientDataJSON: string;
      authenticatorData: string;
      signature: string;
      userHandle?: string;
    };
  }> {
    try {
      const credential = await startAuthentication(options);
      return {
        id: credential.id,
        response: {
          clientDataJSON: credential.response.clientDataJSON,
          authenticatorData: credential.response.authenticatorData,
          signature: credential.response.signature,
          userHandle: (credential.response as any).userHandle,
        },
      };
    } catch (error) {
      console.error("WebAuthn 认证失败:", error);
      throw error;
    }
  },
};

export default webauthn;
