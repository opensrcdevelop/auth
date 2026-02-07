import { startAuthentication, startRegistration } from "@simplewebauthn/browser";

/**
 * Uint8Array 转换为 Base64URL 字符串
 */
function uint8ArrayToBase64Url(uint8Array: Uint8Array): string {
  const base64 = btoa(String.fromCharCode.apply(null, uint8Array as any));
  return base64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

/**
 * Uint8Array 转换为标准 Base64 字符串（用于后端 webAuthn4j）
 */
function uint8ArrayToBase64(uint8Array: Uint8Array): string {
  return btoa(String.fromCharCode.apply(null, uint8Array as any));
}

/**
 * ArrayBuffer 转换为 Base64URL 字符串
 */
function arrayBufferToBase64Url(buffer: ArrayBuffer | string | undefined): string {
  if (!buffer) {
    return "";
  }
  if (typeof buffer === "string") {
    return buffer;
  }
  const uint8Array = new Uint8Array(buffer);
  return uint8ArrayToBase64Url(uint8Array);
}

/**
 * ArrayBuffer 转换为标准 Base64 字符串（用于后端 webAuthn4j）
 */
function arrayBufferToBase64(buffer: ArrayBuffer | string | undefined): string {
  if (!buffer) {
    return "";
  }
  if (typeof buffer === "string") {
    return buffer;
  }
  const uint8Array = new Uint8Array(buffer);
  return uint8ArrayToBase64(uint8Array);
}

/**
 * 将后端返回的注册选项转换为 @simplewebauthn 期望的格式
 * 注意：@simplewebauthn/browser 会自动将 Base64URL 字符串转换为 ArrayBuffer
 */
function convertToWebAuthnOptions(options: any): any {
  if (!options) {
    throw new Error("WebAuthn 注册选项为空");
  }

  // 处理 excludeCredentials：后端返回的是字符串数组，需要转换为对象数组
  const excludeCredentials = options.excludeCredentials?.map((credId: string) => ({
    id: credId, // 保持字符串格式，由 @simplewebauthn 自动转换
    type: "public-key" as const,
  })) || [];

  // challenge 和 user.id 保持字符串格式，由 @simplewebauthn 自动转换
  return {
    challenge: options.challenge,
    rp: {
      id: options.rp?.id,
      name: options.rp?.name,
    },
    user: {
      id: options.user?.id,
      name: options.user?.name,
      displayName: options.user?.displayName,
    },
    pubKeyCredParams: options.pubKeyCredParams?.map((param: any) => ({
      type: param.type,
      alg: param.alg,
    })) || [],
    timeout: options.timeout,
    authenticatorSelection: options.authenticatorSelection,
    excludeCredentials,
  };
}

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
   * @param options 后端返回的注册选项
   * @returns 认证响应
   */
  async startRegistration(options: any): Promise<{
    id: string;
    rawId: string;
    response: {
      clientDataJSON: string;
      attestationObject: string;
      transports?: string[];
    };
  }> {
    try {
      // 转换选项格式
      const webAuthnOptions = convertToWebAuthnOptions(options);

      // 调用浏览器 API
      const { startRegistration: browserStartRegistration } = await import("@simplewebauthn/browser");
      const credential = await browserStartRegistration({
        optionsJSON: webAuthnOptions,
      });
      return {
        id: credential.id,
        rawId: arrayBufferToBase64Url(credential.rawId),
        response: {
          clientDataJSON: arrayBufferToBase64Url(credential.response.clientDataJSON),
          // 后端 webAuthn4j 使用 Jackson 解析，需要标准 Base64
          attestationObject: arrayBufferToBase64(credential.response.attestationObject),
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
    rawId: string;
    response: {
      clientDataJSON: string;
      authenticatorData: string;
      signature: string;
      userHandle?: string;
    };
  }> {
    try {
      // @simplewebauthn/browser 需要 optionsJSON 包装
      const credential = await startAuthentication({ optionsJSON: options });
      return {
        id: credential.id,
        rawId: arrayBufferToBase64Url(credential.rawId),
        response: {
          clientDataJSON: arrayBufferToBase64Url(credential.response.clientDataJSON),
          authenticatorData: arrayBufferToBase64Url(credential.response.authenticatorData),
          signature: arrayBufferToBase64Url(credential.response.signature),
          userHandle: arrayBufferToBase64Url((credential.response as any).userHandle),
        },
      };
    } catch (error) {
      console.error("WebAuthn 认证失败:", error);
      throw error;
    }
  },
};

export default webauthn;
