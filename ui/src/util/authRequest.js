import Request from "./http";
import { getOAuthIssuer } from "./tool";
var request = new Request({
    // 认证服务地址
    baseURL: getOAuthIssuer(),
    timeout: 60 * 1000,
    withCredentials: true,
}, import.meta.env.VITE_API_BASE_URI);
export default request;
