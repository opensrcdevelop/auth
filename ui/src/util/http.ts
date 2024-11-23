import axios from "axios";
import type {
  AxiosInstance,
  AxiosRequestConfig,
  InternalAxiosRequestConfig,
  AxiosResponse,
} from "axios";
import pinia from "@/store";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import router from "@/router";
import { base64Str, getOAuthIssuer } from "./tool";
import { Notification } from "@arco-design/web-vue";

type Result<T> = {
  success: boolean;
  code: number;
  message: string;
  data: T;
};

const globalVariables = useGlobalVariablesStore(pinia);

export class Request {
  // axios 实例
  instance: AxiosInstance;
  // 基础配置
  baseConfig: AxiosRequestConfig = {};
  // 前缀
  prefix: string = "";

  constructor(config: AxiosRequestConfig, prefix: string = "") {
    this.instance = axios.create(Object.assign(this.baseConfig, config));
    this.prefix = prefix;

    this.instance.interceptors.request.use(
      (config: InternalAxiosRequestConfig) => {
        globalVariables.apiLoading = true;
        config.baseURL = prefix ? `${getOAuthIssuer()}${prefix}` : getOAuthIssuer();

        const accessToken = localStorage.getItem("accessToken");
        if (accessToken) {
          const token: any = JSON.parse(accessToken);
          if (token && config.url !== "/oauth2/token" && !config.url.startsWith("/tenant/check/")) {
            config.headers!.Authorization = `${token.token_type} ${token.access_token}`;
          }
        }
        return config;
      },
      (err: any) => {
        return Promise.reject(err);
      }
    );

    this.instance.interceptors.response.use(
      (res: AxiosResponse) => {
        globalVariables.apiLoading = false;

        return res.data;
      },
      (err: any) => {
        globalVariables.apiLoading = false;

        if (err.code === "ERR_NETWORK") {
          return Promise.reject(err);
        }
        let messageText = "";
        switch (err.response.status) {
          case 400:
            messageText = "请求错误(400)";
            break;
          case 401:
            messageText = "未授权，请重新登录(401)";
            if (err.config.url !== "/oauth2/token" && hanlde401(this.instance)) {
              // 重新请求
              return this.instance.request(err.config);
            } else {
              break;
            }
          case 403:
            messageText = "拒绝访问(403)";
            break;
          case 404:
            messageText = "请求路径出错(404)";
            break;
          case 408:
            messageText = "请求超时(408)";
            break;
          case 500:
            messageText = "服务器错误(500)";
            break;
          case 501:
            messageText = "服务未实现(501)";
            break;
          case 502:
            messageText = "网络错误(502)";
            break;
          case 503:
            messageText = "服务不可用(503)";
            break;
          case 504:
            messageText = "网络超时(504)";
            break;
          case 505:
            messageText = "HTTP版本不受支持(505)";
            break;
          default:
            messageText = `连接出错(${err.response.status})!`;
        }
        err.response.statusText = messageText;
        return Promise.reject(err.response);
      }
    );
  }

  // 定义请求方法
  request<T>(config: AxiosRequestConfig): Promise<AxiosResponse<T>> {
    return this.instance.request(config);
  }

  get<T>(config?: AxiosRequestConfig<T>): Promise<AxiosResponse<T>> {
    return this.request<T>({ ...config, method: "GET" });
  }

  post<T>(config?: AxiosRequestConfig<T>): Promise<AxiosResponse<T>> {
    return this.request<T>({ ...config, method: "POST" });
  }

  delete<T>(config?: AxiosRequestConfig<T>): Promise<AxiosResponse<T>> {
    return this.request<T>({ ...config, method: "DELETE" });
  }

  put<T>(config?: AxiosRequestConfig<T>): Promise<AxiosResponse<T>> {
    return this.request<T>({ ...config, method: "PUT" });
  }

  patch<T>(config?: AxiosRequestConfig<T>): Promise<AxiosResponse<T>> {
    return this.request<T>({ ...config, method: "PATCH" });
  }
}

/**
 * 处理 401
 */
async function hanlde401(axios: AxiosInstance) {
  // 使用 refresh_token 重新获取 access_token
  const accessToken = localStorage.getItem("accessToken");
  if (accessToken) {
    const accessTokenJson = JSON.parse(accessToken);
    if (accessTokenJson.refresh_token) {
      try {
        const res = await axios.request({
          baseURL: getOAuthIssuer(),
          url: "/oauth2/token",
          method: "POST",
          headers: {
            "Content-Type": "application/x-www-form-urlencoded",
            Authorization: `Basic ${base64Str(
              `${import.meta.env.VITE_OAUTH_CLIENT_ID}:${
                import.meta.env.VITE_OAUTH_CLIENT_SECRET
              }`
            )}`,
          },
          data: {
            grant_type: "refresh_token",
            refresh_token: accessTokenJson.refresh_token,
          },
        });
        localStorage.setItem("accessToken", JSON.stringify(res));
        return true;
      } catch (err) {
        Notification.error("刷新 token 失败")
        // 如果获取失败，则跳转到登录页面
        router.push({
          path: "/oauth2/redirect",
        });
        localStorage.removeItem("accessToken");
        return false;
      }
    }
  }
}

// 默认导出Request实例
export default Request;
