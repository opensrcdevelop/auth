<script lang="ts">
import homeTs from "./index";

export default homeTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <a-spin
    class="user-center-container"
    :loading="globalVariables.apiLoading"
    tip="处理中，请稍候..."
  >
    <div class="header">
      <div class="left">
        <div class="logo">
          <img src="/logo.png" class="logo-img" />
        </div>
        <a-divider direction="vertical" />
        <span class="title">个人中心</span>
      </div>
      <div class="right">
        <div v-if="consoleAccess" class="console" @click="handleToConsole">
          <icon-desktop style="margin-right: 6px; font-size: 14px" />
          <span>控制台</span>
        </div>
        <a-dropdown position="br">
          <a-avatar :style="{ backgroundColor: '#396aff' }" v-if="username">{{
            username
          }}</a-avatar>
          <a-avatar :style="{ backgroundColor: '#396aff' }" v-else>
            <icon-user />
          </a-avatar>
          <template #content>
            <a-doption @click="handleOpenChangePwdModal">
              <icon-lock style="margin-right: 6px" />
              <span>修改密码</span>
            </a-doption>
            <a-doption @click="handleLogout">
              <icon-poweroff style="margin-right: 6px" />
              <span>退出登录</span>
            </a-doption>
          </template>
        </a-dropdown>
      </div>
    </div>
    <div class="main">
      <div class="tabs-container">
        <a-tabs
          style="height: 100%"
          position="left"
          :active-key="activeTab"
          @change="handleTabChange"
        >
          <a-tab-pane key="user_info" title="个人信息">
            <MyUserInfo
              :userInfo="userInfo"
              :activeKey="activeTab"
              @userInfoUpdated="handleUserInfoUpdated"
            />
          </a-tab-pane>
          <a-tab-pane key="account_binding" title="账号绑定">
            <AccountBinding :userInfo="userInfo" :activeKey="activeTab" />
          </a-tab-pane>
          <a-tab-pane key="my_permissions" title="我的权限">
            <MyPermissions :activeKey="activeTab" />
          </a-tab-pane>
          <a-tab-pane key="apply_permission" title="申请权限">
            <ApplyPermission :activeKey="activeTab" />
          </a-tab-pane>
          <a-tab-pane key="request_records" title="申请权限记录">
            <RequestRecords :userInfo="userInfo" :activeKey="activeTab" />
          </a-tab-pane>
        </a-tabs>
      </div>
    </div>
  </a-spin>

  <!-- 修改密码对话框 -->
  <a-modal
    :visible="changePwdModalVisible"
    :footer="false"
    @cancel="handleCloseChangePwdModal"
  >
    <template #title>修改密码</template>
    <a-form
      :model="changePwdForm"
      :rules="changePwdFormRules"
      ref="changePwdFormRef"
      layout="vertical"
      @submit-success="handleSubmitChangePwdForm"
    >
      <a-form-item field="rawPwd" label="原密码">
        <a-input-password
          v-model="changePwdForm.rawPwd"
          placeholder="请输入原密码"
        />
      </a-form-item>
      <a-form-item field="newPwd" label="新密码">
        <password-checker
          ref="passwordCheckerRef"
          type="password"
          placeholder="请输入新密码"
          :loading="checkPasswordLoading"
          @check="handleCheckPassword"
          :checkRes="checkPasswordRes"
        />
      </a-form-item>
      <a-form-item field="confirmPwd" label="确认密码">
        <a-input-password
          v-model="changePwdForm.confirmPwd"
          placeholder="请确认密码"
        />
      </a-form-item>
      <a-form-item hide-label>
        <div class="btn-container">
          <a-space>
            <a-button @click="handleCloseChangePwdModal">取消</a-button>
            <a-button
              type="primary"
              html-type="submit"
              :loading="changePwdFormSubmitLoading"
              >确定</a-button
            >
          </a-space>
        </div>
      </a-form-item>
    </a-form>
  </a-modal>
</template>
