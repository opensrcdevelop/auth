import type {AxiosInstance, AxiosRequestConfig, AxiosResponse, InternalAxiosRequestConfig,} from "axios";
import axios from "axios";
import pinia from "@/store";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import router from "@/router";
import {base64Str, decodeBase64Str, getOAuthIssuer} from "./tool";
import {Notification} from "@arco-design/web-vue";
import {AUTH_TOKENS, REDIRECT_PATH, REDIRECT_QUERY, REFRESH_TOKEN,} from "./constants";

const globalVariables = useGlobalVariablesStore(pinia);

export class Request {
  // axios 实例
  instance: AxiosInstance;
  // 基础配置
  baseConfig: AxiosRequestConfig = {};
  // 前缀
  prefix: string = "";
  // 是否开启全局 Loading
  showGlobalLoading: boolean = true;

  constructor(config: AxiosRequestConfig, prefix: string = "", showGlobalLoading: boolean = true) {
    this.instance = axios.create(Object.assign(this.baseConfig, config));
    this.prefix = prefix;
    this.showGlobalLoading = showGlobalLoading;

    this.instance.interceptors.request.use(
      (config: InternalAxiosRequestConfig) => {
        if (this.showGlobalLoading) {
          globalVariables.apiLoading = true;
        }

        config.baseURL = prefix
          ? `${getOAuthIssuer()}${prefix}`
          : getOAuthIssuer();

        const authTokens = localStorage.getItem(AUTH_TOKENS);
        if (authTokens) {
          const token: any = JSON.parse(decodeBase64Str(authTokens));
          if (
            token &&
            config.url !== "/oauth2/token" &&
            !config.url.startsWith("/tenant/check/") &&
            !config.url.startsWith("/captcha") &&
            !config.url.startsWith("/identitySource/enabled")
          ) {
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
        if (this.showGlobalLoading) {
          globalVariables.apiLoading = false;
        }

        if (res.data) {
          return res.data;
        }
        return res;
      },
      (err: any) => {
        if (this.showGlobalLoading) {
          globalVariables.apiLoading = false;
        }

        if (err.response) {
          let messageText = "";
          switch (err.response.status) {
            case 400:
              messageText = "请求错误(400)";
              break;
            case 401:
              messageText = "未授权，请重新登录(401)";
              if (err.config.url !== "/oauth2/token") {
                return refreshingToken(this.instance, err.config);
              }
              break;
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

        return Promise.reject(err);
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

// 是否正在刷新标记
let isRefreshing = false;
// 重试队列
let retryRequests = [];

/**
 * 刷新 token
 */
async function refreshingToken(
  axios: AxiosInstance,
  requestConfig: AxiosRequestConfig
) {
  if (!isRefreshing) {
    // 使用 refresh_token 重新获取 access_token
    const authTokens = localStorage.getItem(AUTH_TOKENS);
    if (authTokens) {
      const authTokensJson = JSON.parse(decodeBase64Str(authTokens));
      if (authTokensJson.refresh_token) {
        isRefreshing = true;
        try {
          const res: any = await axios.request({
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
              grant_type: REFRESH_TOKEN,
              refresh_token: authTokensJson.refresh_token,
            },
          });
          localStorage.setItem(AUTH_TOKENS, base64Str(JSON.stringify(res)));
          // 重试刷新 token 间的所有请求
          retryRequests.forEach((cb) => cb(res));
          isRefreshing = false;
          retryRequests = [];

          // 重试当前请求
          resetToken(res, requestConfig);
          return axios(requestConfig);
        } catch (err) {
          Notification.error("刷新 token 失败");
          isRefreshing = false;
          retryRequests = [];
          localStorage.removeItem(AUTH_TOKENS);
          handleRelogin();
        }
      }
    }
  } else {
    return new Promise((resolve) => {
      // 等待刷新 token 完成后执行
      retryRequests.push((token) => {
        resetToken(token, requestConfig);
        resolve(axios(requestConfig));
      });
    });
  }
}

/**
 * 重新登录
 */
function handleRelogin() {
  // 存储当前页面路径及 query，方便登录后跳转
  const currentRoute = router.currentRoute.value;
  if (currentRoute && !localStorage.getItem(REDIRECT_PATH)) {
    localStorage.setItem(REDIRECT_PATH, currentRoute.path);
    if (currentRoute.query && !localStorage.getItem(REDIRECT_QUERY)) {
      localStorage.setItem(REDIRECT_QUERY, JSON.stringify(currentRoute.query));
    }
  }
  router.push({
    path: "/oauth2/redirect",
  });
}

/**
 * 重新设置token
 */
function resetToken(token: any, requestConfig: AxiosRequestConfig) {
  requestConfig.headers!.Authorization = `${token.token_type} ${token.access_token}`;
}

// 默认导出Request实例
export default Request;
