import Request from "./http";
import { getOAuthIssuer } from "./tool";

const request = new Request({
  // 认证服务地址
  baseURL: getOAuthIssuer(),
  timeout: 60 * 1000,
  withCredentials: true,
});

export default request;
