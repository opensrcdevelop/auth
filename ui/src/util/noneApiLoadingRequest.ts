import Request from "./http";

const noneLoadingApiRequest = new Request({
  timeout: 60 * 1000,
  withCredentials: true,
}, import.meta.env.VITE_API_BASE_URI, false);

export default noneLoadingApiRequest;
