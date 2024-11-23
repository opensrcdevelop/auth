<script lang="ts">
import createTs from "./index";
export default createTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建用户</div>
      <div class="info-title">用户信息</div>
      <a-form
        :model="createUserForm"
        layout="vertical"
        :rules="createUserFormRules"
        ref="createUserFormRef"
        @submit-success="handleCreateUserFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="username" label="用户名">
              <a-input
                v-model="createUserForm.username"
                placeholder="请输入用户名"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="password" label="密码">
              <a-input-group style="width: 100%">
                <a-input-password
                  v-model="createUserForm.password"
                  placeholder="请输入密码"
                />
                <a-button @click="handleGeneratePassword">生成密码</a-button>
              </a-input-group>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="phoneNumber" label="手机号">
              <a-input
                v-model="createUserForm.phoneNumber"
                placeholder="请输入手机号"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="emailAddress" label="邮箱">
              <a-input
                v-model="createUserForm.emailAddress"
                placeholder="请输入邮箱"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item
              field="needChangePwd"
              label="强制用户在首次登录时修改密码"
            >
              <a-switch type="round" v-model="createUserForm.needChangePwd" />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="sendEmail" label="发送邮件通知用户">
              <a-switch type="round" v-model="createUserForm.sendEmail" />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateUserForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
