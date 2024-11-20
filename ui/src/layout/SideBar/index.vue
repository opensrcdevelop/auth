<template>
  <a-layout-sider class="layout-sider">
    <Menu class="menu"></Menu>
    <div class="action-container">
      <a-avatar :style="{ backgroundColor: '#396aff' }"
        >{{ currentUser.username }}</a-avatar
      >
      <a-trigger position="right" :popup-translate="[10, -10]">
        <a-button type="text" size="large">
          <template #icon>
            <icon-more />
          </template>
        </a-button>
        <template #content>
          <div class="operation-container">
            <div class="operation-item" @click="handleToUserCenter">
              <icon-user />
              个人中心
            </div>
            <div class="operation-item" @click="handleLogout">
              <icon-poweroff />
              退出
            </div>
          </div>
        </template>
      </a-trigger>
    </div>
  </a-layout-sider>
</template>

<script setup lang="ts">
import Menu from "./components/Menu.vue";
import { computed, onMounted, reactive } from "vue";
import { getCurrentUser } from "@/api/user";
import { handleApiSuccess, handleApiError } from "@/util/tool";
import { logoutSubmit } from "@/api/logout";
import { Modal, Notification } from "@arco-design/web-vue";
import router from "@/router";

/** 当前用户信息 */
const currentUser = reactive({
  id: "",
  username: "",
  consoleAccess: undefined,
});

/**
 * 获取当前用户信息
 */
const handleGetCurrentUser = () => {
  getCurrentUser()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        currentUser.id = data.id;
        currentUser.username = data.username;
        currentUser.consoleAccess = data.consoleAccess;

        checkConsoleAccess();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取当前用户信息");
    });
};

/**
 * 退出登录
 */
const handleLogout = () => {
  Modal.warning({
    title: "确定退出登录？",
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "warning",
    },
    onOk: () => {
      logoutSubmit()
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("退出成功");
            localStorage.removeItem("accessToken");

            // 跳转到登录页
            router.push({
              path: "/oauth2/redirect",
            });
          });
        })
        .catch((err: any) => {
          handleApiError(err, "退出登录");
        });
    },
  });
};

/**
 * 跳转至个人中心
 */
const handleToUserCenter = () => {
  router.push({
    path: "/user/home",
  })
}

/**
 * 检查控制台访问权限
 */
const checkConsoleAccess = () => {
  if (!currentUser.consoleAccess) {
    router.push({
      path: "/403",
    })
  }
}

onMounted(() => {
  handleGetCurrentUser();
});
</script>

<style lang="scss" scoped>
.operation-container {
  background-color: #fff;
  box-shadow: 0 0 12px 0 #eceef4;
  border-radius: 8px;
  padding: 8px;
  box-sizing: border-box;

  .operation-item {
    width: 120px;
    border-radius: 4px;
    padding: 8px 16px;
    color: #293350;
    cursor: pointer;
    line-height: 22px;
    font-weight: 400;
  }

  .operation-item:hover {
    background-color: #eff2f6;
  }
}
</style>
