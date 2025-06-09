<template>
  <div class="loading-container" v-if="loading">
    <a-skeleton animation>
      <a-skeleton-line :rows="3" />
    </a-skeleton>
  </div>
  <div v-if="federationLoginEnabled" class="federation-login-container">
    <div class="federation-login-title">第三方账号登录</div>
    <div
      class="federation-login-button-container"
      v-for="item in identitySourceList"
    >
      <a-button
        :key="item.code"
        class="federation-login-button"
        @click="handleOpenAuthWindow(item.authorizationUri)"
      >
        <div class="federation-login-button-content">
          <img class="logo" :src="item.logo" :draggable="false" />
          <span class="text">通过 {{ item.name }} 继续</span>
        </div>
      </a-button>
    </div>
  </div>
</template>

<script setup lang="ts">
import {getEnabledIdentitySource} from "@/api/identitySource";
import router from "@/router";
import {AUTH_FAILURE, AUTH_SUCCESS, USER_LOCKED} from "@/util/constants";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {computed, onMounted, reactive, ref} from "vue";

// 身份源列表
const identitySourceList = reactive([]);
// 是否启用第三方登录
const federationLoginEnabled = computed(() => {
  return identitySourceList.length > 0;
});
const loading = ref(false);

onMounted(() => {
  handleGetEnabledIdentitySource();
  window.addEventListener("message", handleAuthWindowResponse);
});

/**
 * 打开第三方认证窗口
 */
var authWindow;
const handleOpenAuthWindow = (authorizationUri: string) => {
  authWindow = window.open(authorizationUri, "_blank", "width=600,height=600");
};

/**
 * 处理第三方认证窗口响应
 */
const handleAuthWindowResponse = (event) => {
  if (event.data === AUTH_SUCCESS) {
    Notification.success("第三方账号登录成功");
    authWindow.close();
    toTarget();
  }

  if (event.data === AUTH_FAILURE) {
    Notification.error("第三方账号登录失败");
    authWindow.close();
  }

  if (event.data === USER_LOCKED) {
    Notification.error("账号已被禁用，无法通过第三方账号登录");
    authWindow.close();
  }
};

/**
 * 跳转至目标路径
 */
const toTarget = () => {
  let target = getQueryString("target");
  if (target) {
    window.location.href = target;
  } else {
    router.push({
      path: "/",
    });
  }
};
const handleGetEnabledIdentitySource = () => {
  loading.value = true;
  getEnabledIdentitySource()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        identitySourceList.length = 0;
        identitySourceList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取启用的身份源");
    })
    .finally(() => {
      loading.value = false;
    });
};
</script>

<style lang="scss" scoped>
.loading-container {
  margin-top: 16px;
}

.federation-login-container {
  width: 100%;
  height: 100%;
  margin-top: 16px;
  display: flex;
  flex-direction: column;
  align-items: center;
  user-select: none; 

  .federation-login-title {
    position: relative;
    color: #c8c9cc;
    font-size: 14px;
    margin-bottom: 12px;
  }

  .federation-login-title::before,
  .federation-login-title::after {
    content: "";
    position: absolute;
    top: 50%;
    width: 50px;
    height: 1px;
    background-color: #eee;
  }

  .federation-login-title::before {
    left: 0;
    transform: translate(-120%, -50%);
  }

  .federation-login-title::after {
    right: 0;
    transform: translate(120%, -50%);
  }

  .federation-login-button-container {
    width: 100%;

    .federation-login-button {
      width: 100%;
      height: 44px;
      margin-bottom: 12px;

      .federation-login-button-content {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: space-between;

        .logo {
          width: 44px;
          height: 44px;
        }

        .text {
          width: 100%;
          color: var(--color-text-1);
        }
      }
    }
  }
}
</style>
