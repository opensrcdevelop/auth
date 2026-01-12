import Request from "./http";
var apiRequest = new Request({
    timeout: 60 * 1000,
    withCredentials: true,
}, import.meta.env.VITE_API_BASE_URI);
export default apiRequest;
