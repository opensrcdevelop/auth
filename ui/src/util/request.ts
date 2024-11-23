import Request from "./http";

const request = new Request({
  timeout: 60 * 1000,
});

export default request;
