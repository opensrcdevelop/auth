<script lang="ts">
import homeTs from "./index";
export default homeTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div class="user-center-container">
    <div class="header">
      <div class="left">
        <div class="logo">
          <img src="/logo.png" class="logo-img" />
        </div>
        <a-divider direction="vertical" />
        <span class="title">个人中心</span>
      </div>
      <div class="right">
        <div class="console" @click="handleToConsole">
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
          position="left"
          :active-key="activeTab"
          @change="handleTabChange"
        >
          <a-tab-pane key="user_info" title="个人信息">
            <div class="card">
              <a-spin :loading="userInfoLoading" style="width: 100%">
                <a-card title="个人信息">
                  <template #extra>
                    <a-button type="text" @click="handleUpdateMyUserInfo">
                      <template #icon>
                        <icon-save />
                      </template>
                      保存
                    </a-button>
                  </template>
                  <a-form :model="userAttrs" layout="vertical">
                    <a-row :gutter="24">
                      <a-col
                        :span="12"
                        v-for="attr in userAttrs"
                        :key="attr.key"
                      >
                        <a-form-item :label="attr.name">
                          <a-input-number
                            v-if="attr.dataType === 'NUMBER'"
                            hide-button
                            v-model="userInfo[attr.key]"
                            :allowClear="attr.userEditable"
                            :disabled="!attr.userEditable"
                            :placeholder="`请输入${attr.name}`"
                          />
                          <a-input
                            v-if="attr.dataType === 'STRING'"
                            v-model="userInfo[attr.key]"
                            :allowClear="attr.userEditable"
                            :disabled="!attr.userEditable"
                            :placeholder="`请输入${attr.name}`"
                          />
                          <a-select
                            v-if="attr.dataType === 'BOOLEAN'"
                            v-model="userInfo[attr.key]"
                            :allowClear="attr.userEditable"
                            :disabled="!attr.userEditable"
                            :placeholder="`请选择${attr.name}`"
                          >
                            <a-option :value="true">是</a-option>
                            <a-option :value="false">否</a-option>
                          </a-select>
                          <a-date-picker
                            style="width: 100%"
                            v-if="attr.dataType === 'DATETIME'"
                            show-time
                            value-format="timestamp"
                            v-model="userInfo[attr.key]"
                            :disabled="!attr.userEditable"
                            :placeholder="`请选择${attr.name}`"
                          />
                          <a-date-picker
                            style="width: 100%"
                            v-if="attr.dataType === 'DATE'"
                            value-format="timestamp"
                            v-model="userInfo[attr.key]"
                            :disabled="!attr.userEditable"
                            :placeholder="`请选择${attr.name}`"
                          />
                          <a-select
                            v-if="attr.dataType === 'DICT'"
                            v-model="userInfo[attr.key]"
                            allowClear
                            :placeholder="`请选择${attr.name}`"
                          >
                            <a-option
                              :value="dictData.id"
                              v-for="dictData in allDictDatas[attr.key]"
                              :key="dictData.id"
                              >{{ dictData.label }}</a-option
                            >
                          </a-select>
                        </a-form-item>
                      </a-col>
                    </a-row>
                  </a-form>
                </a-card>
              </a-spin>
            </div>
          </a-tab-pane>
          <a-tab-pane key="account_binding" title="账号绑定">
            <div class="card">
              <a-card title="手机号和邮箱">
                <div class="binding-card">
                  <div class="icon-container">
                    <div class="icon">
                      <icon-email />
                    </div>
                    <span>邮箱</span>
                    <span
                      v-if="userInfo['emailAddress']"
                      style="color: #396aff; margin-left: 8px"
                      >{{ userInfo["emailAddress"] }}</span
                    >
                  </div>
                  <div class="status-container">
                    <div class="binding" v-if="!userInfo['emailAddress']">
                      <a-button type="text" @click="handleOpenBindEmailModal">
                        <template #icon>
                          <icon-font type="icon-binding" />
                        </template>
                        绑定
                      </a-button>
                    </div>
                    <div class="unbind" v-else>
                      <a-button
                        type="text"
                        status="warning"
                        @click="handleOpenUnbindEmailModal"
                      >
                        <template #icon>
                          <icon-font type="icon-unbind" />
                        </template>
                        解除绑定
                      </a-button>
                    </div>
                  </div>
                </div>
              </a-card>
            </div>
          </a-tab-pane>
        </a-tabs>
      </div>
    </div>
  </div>

  <!-- 修改密码对话框 -->
  <a-modal
    :visible="changePwdModalVisivle"
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
        <a-input-password
          v-model="changePwdForm.newPwd"
          placeholder="请输入新密码"
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

  <!-- 绑定 / 解绑邮箱对话框 -->
  <a-modal
    :visible="bindOrUnbindEmailModalVisivle"
    @cancel="handleCoseBindOrUnbindEmailModal"
    @ok="handleBindOrUnbindEmailFormSubmit"
    :ok-loading="bindOrUnbindEmailFormSubmitLoading"
  >
    <template #title>{{ isBinding ? "绑定邮箱" : "解绑邮箱" }}</template>
    <a-form
      :model="bindOrUnbindEmailForm"
      :rules="bindOrUnbindEmailFormRules"
      ref="bindOrUnbindEmailFormRef"
      layout="vertical"
    >
      <a-form-item field="email" label="邮箱">
        <a-input
          v-model="bindOrUnbindEmailForm.email"
          :readonly="!isBinding"
          placeholder="请输入邮箱"
        />
      </a-form-item>
      <a-form-item field="code" label="验证码">
        <a-input-group style="width: 100%">
          <a-input
            v-model="bindOrUnbindEmailForm.code"
            placeholder="请输入验证码"
          />
          <a-button
            type="primary"
            :disabled="sendEmailCodeDisable"
            @click="handleSendEmailCode"
            >{{ sendEmailCodeBtnText }}</a-button
          >
        </a-input-group>
      </a-form-item>
    </a-form>
  </a-modal>
</template>
