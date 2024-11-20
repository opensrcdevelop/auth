<script setup lang="ts">
import {
  generateRandomString,
  getQueryString,
  generateCodeChallenge,
  getOAuthIssuer,
  getConsoleUrl,
  handleApiSuccess,
  handleApiError,
} from "@/util/tool";
import { getToken } from "@/api/login";
import router from "@/router";
import { ref } from "vue";
import { logoutSubmit } from "@/api/logout";

// 获取地址栏授权码
const code = getQueryString("code");

const loading = ref(true);
const hasError = ref(false);
const errorText = ref("");

if (code) {
  // 校验 state，防止 CSRF
  const state = localStorage.getItem("state");
  const urlState = getQueryString("state");
  if (urlState !== state) {
    hasError.value = true;
    errorText.value = "state 不匹配，可能存在 CSRF 攻击";
  } else {
    // 获取 token
    getToken({
      grant_type: "authorization_code",
      client_id: import.meta.env.VITE_OAUTH_CLIENT_ID,
      client_secret: import.meta.env.VITE_OAUTH_CLIENT_SECRET,
      redirect_uri: `${getConsoleUrl()}${import.meta.env.VITE_UI_BASE_PATH || ''}/oauth2/redirect`,
      code,
      code_verifier: localStorage.getItem("codeVerifier"),
      state,
    })
      .then((res: any) => {
        localStorage.setItem("accessToken", JSON.stringify(res));
        localStorage.removeItem("state");
        localStorage.removeItem("codeVerifier");
        router.push({ path: "/" });
      })
      .catch((err: any) => {
        hasError.value = true;
        errorText.value = err.data.message || err.statusText;
      });
  }
} else {
  // 生成 state
  let state: string = generateRandomString(32);
  // 生成 CodeVerifier
  let codeVerifier: string = generateRandomString(32);
  // 生成 CodeChallenge
  let codeChallenge: string = generateCodeChallenge(codeVerifier);

  // 缓存 state 和 codeVerifier
  localStorage.setItem("state", state);
  localStorage.setItem("codeVerifier", codeVerifier);

  // 获取授权码
  window.location.href = `${getOAuthIssuer()}/oauth2/authorize?client_id=${
    import.meta.env.VITE_OAUTH_CLIENT_ID
  }&response_type=code&scope=openid&redirect_uri=${getConsoleUrl()}${import.meta.env.VITE_UI_BASE_PATH || ''}/oauth2/redirect&code_challenge=${codeChallenge}&code_challenge_method=S256&state=${state}`;
}

loading.value = false;

const handleRetry = () => {
  logoutSubmit()
    .then((result: any) => {
      handleApiSuccess(result, () => {
       window.location.href = `${getConsoleUrl()}${import.meta.env.VITE_UI_BASE_PATH || ''}`;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "退出登录");
    });
};
</script>

<template>
  <div class="result-container">
    <a-spin style="width: 100%" :loading="loading" tip="Loading...">
      <a-result status="warning" v-if="hasError">
        <template #title>
          <div class="title">token 获取失败</div>
        </template>
        <template #subtitle> 错误详情： {{ errorText }} </template>
        <template #extra>
          <a-button type="text" @click="handleRetry">
            <template #icon>
              <icon-refresh />
            </template>
            重试
          </a-button>
        </template>
      </a-result>
    </a-spin>
  </div>
</template>

<style scoped lang="scss">
.result-container {
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;

  .title {
    font-size: 18px;
    font-weight: 400;
    color: #1d2129;
  }
}
</style>
