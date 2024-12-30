<script setup lang="ts">
import {
  base64Str,
  generateCodeChallenge,
  generateRandomString,
  getConsoleUrl,
  getOAuthIssuer,
  getQueryString,
  handleApiError,
  handleApiSuccess,
} from "@/util/tool";
import {getToken} from "@/api/login";
import router from "@/router";
import {ref} from "vue";
import {logoutSubmit} from "@/api/logout";
import {
  AUTH_TOKENS,
  AUTHORIZAION_CODE,
  CODE,
  CODE_VERIFIER,
  REDIRECT_PATH,
  REDIRECT_QUERY,
  STATE
} from "@/util/constants";

// 获取地址栏授权码
const code = getQueryString(CODE);

const loading = ref(true);
const hasError = ref(false);
const errorText = ref("");

if (code) {
  // 校验 state，防止 CSRF
  const state = localStorage.getItem(STATE);
  const urlState = getQueryString(STATE);
  if (urlState !== state) {
    hasError.value = true;
    errorText.value = "state 不匹配，可能存在 CSRF 攻击";
  } else {
    // 获取 token
    getToken({
      grant_type: AUTHORIZAION_CODE,
      client_id: import.meta.env.VITE_OAUTH_CLIENT_ID,
      client_secret: import.meta.env.VITE_OAUTH_CLIENT_SECRET,
      redirect_uri: `${getConsoleUrl()}${import.meta.env.VITE_UI_BASE_PATH || ''}/oauth2/redirect`,
      code,
      code_verifier: localStorage.getItem(CODE_VERIFIER),
      state,
    })
      .then((res: any) => {
        localStorage.setItem(AUTH_TOKENS, base64Str(JSON.stringify(res)));
        localStorage.removeItem(STATE);
        localStorage.removeItem(CODE_VERIFIER);

        const redirectPath = localStorage.getItem(REDIRECT_PATH);
        const redirectQuery = localStorage.getItem(REDIRECT_QUERY);
        // 跳转到目标路径
        if (localStorage.getItem(REDIRECT_PATH)) {
          if (redirectQuery) {
            router.push({
              path: redirectPath,
              query: JSON.parse(redirectQuery),
            })
          } else {
            router.push({ path: redirectPath })
          }
          localStorage.removeItem(REDIRECT_PATH);
          localStorage.removeItem(REDIRECT_QUERY);
        } else {
          // 跳转到首页
          router.push({ path: "/" });
        }
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
  localStorage.setItem(STATE, state);
  localStorage.setItem(CODE_VERIFIER, codeVerifier);

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
