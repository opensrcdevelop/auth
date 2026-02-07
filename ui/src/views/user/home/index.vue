<script lang="ts">
import homeTs from "./index";

export default homeTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
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
          position="left"
          :active-key="activeTab"
          @change="handleTabChange"
        >
          <a-tab-pane key="user_info" title="个人信息">
            <div class="card">
              <a-spin
                :loading="loading"
                style="width: 100%"
                tip="处理中，请稍后..."
              >
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
                            v-if="attr.dataType === 'DICT' && !attr.cascadeDict"
                            v-model="userInfo[attr.key]"
                            allow-clear
                            allow-search
                            :disabled="!attr.userEditable"
                            :placeholder="`请选择${attr.name}`"
                          >
                            <a-option
                              :value="dictData.id"
                              v-for="dictData in allDictDatas[attr.key]"
                              :key="dictData.id"
                              >{{ dictData.label }}</a-option
                            >
                          </a-select>
                          <a-cascader
                            v-if="attr.dataType === 'DICT' && attr.cascadeDict"
                            v-model="userInfo[attr.key]"
                            :placeholder="`请选择${attr.name}`"
                            expand-trigger="hover"
                            :options="allDictDatas[attr.key]"
                            :field-names="{ value: 'id', label: 'label' }"
                            allow-clear
                            allow-search
                          />
                        </a-form-item>
                      </a-col>
                    </a-row>
                  </a-form>
                </a-card>
              </a-spin>
            </div>
          </a-tab-pane>
          <a-tab-pane key="account_binding" title="账号绑定">
            <a-spin
              :loading="loading"
              style="width: 100%"
              tip="处理中，请稍后..."
            >
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
              <div class="card" v-if="boundIdentitySource.length > 0">
                <a-card title="第三方账号">
                  <div
                    class="binding-card"
                    v-for="identitySource in boundIdentitySource"
                    :key="identitySource.id"
                  >
                    <div class="icon-container">
                      <div class="icon">
                        <img
                          class="identity-source-logo"
                          :src="identitySource.logo"
                          :draggable="false"
                        />
                      </div>
                      <div class="name-container">
                        <span>{{ identitySource.name }}</span>
                        <span
                          class="username"
                          v-if="identitySource.bindUsername"
                          >用户名：{{ identitySource.bindUsername }}</span
                        >
                      </div>
                    </div>
                    <div class="status-container">
                      <div class="binding" v-if="!identitySource.isBind">
                        <a-button
                          type="text"
                          @click="handleBindUser(identitySource)"
                        >
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
                          @click="handleUnbindUser(identitySource)"
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
              <!-- WebAuthn/Passkey 凭证管理 -->
              <div class="card">
                <a-card title="Passkey 凭证">
                  <template #extra>
                    <a-button
                      type="text"
                      @click="handleAddWebAuthnCredential"
                      :loading="addingWebAuthnCredential"
                    >
                      <template #icon>
                        <icon-plus />
                      </template>
                      添加凭证
                    </a-button>
                  </template>
                  <a-table
                    :data="webAuthnCredentials"
                    :bordered="false"
                    :pagination="false"
                  >
                    <template #columns>
                      <a-table-column title="凭证 ID" ellipsis tooltip>
                        <template #cell="{ record }">
                          {{ record.id }}
                        </template>
                      </a-table-column>
                      <a-table-column title="设备类型">
                        <template #cell="{ record }">
                          <a-tag
                            v-if="record.deviceType === 'platform'"
                            color="arcoblue"
                          >
                            平台设备
                          </a-tag>
                          <a-tag
                            v-else-if="record.deviceType === 'cross-platform'"
                            color="green"
                          >
                            跨平台设备
                          </a-tag>
                          <a-tag v-else>{{ record.deviceType }}</a-tag>
                        </template>
                      </a-table-column>
                      <a-table-column title="创建时间">
                        <template #cell="{ record }">
                          {{ record.createdAt ? record.createdAt : "-" }}
                        </template>
                      </a-table-column>
                      <a-table-column title="最后使用">
                        <template #cell="{ record }">
                          {{ record.lastUsedAt ? record.lastUsedAt : "-" }}
                        </template>
                      </a-table-column>
                      <a-table-column title="操作" :width="80">
                        <template #cell="{ record }">
                          <a-popconfirm
                            type="warning"
                            content="确定删除此凭证吗？删除后无法使用该设备登录。"
                            :ok-button-props="{ status: 'danger' }"
                            @ok="handleDeleteWebAuthnCredential(record)"
                          >
                            <a-button type="text" status="danger" size="small">
                              删除
                            </a-button>
                          </a-popconfirm>
                        </template>
                      </a-table-column>
                    </template>
                  </a-table>
                </a-card>
              </div>
            </a-spin>
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
