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
      <div class="create-tile">创建客户端</div>
      <div class="info-title">客户端信息</div>
      <a-form
        :model="createClientForm"
        layout="vertical"
        ref="createClientFormRef"
        :rules="createClientFormRules"
        @submit-success="handleCreateClientFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="客户端名称">
              <a-input
                v-model="createClientForm.name"
                placeholder="请输入客户端名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="redirectUri" label="登录回调 URL">
              <a-input
                v-model="createClientForm.redirectUri"
                placeholder="请输入登录回调 URL"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="客户端描述">
          <a-textarea
            v-model="createClientForm.desc"
            placeholder="请输入客户端描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item field="grantTypes" label="授权模式">
          <a-checkbox-group v-model="createClientForm.grantTypes">
            <a-checkbox value="authorization_code"
              >authorization_code</a-checkbox
            >
            <a-checkbox value="refresh_token">refresh_token</a-checkbox>
            <a-checkbox value="client_credentials"
              >client_credentials</a-checkbox
            >
            <a-checkbox value="password">password</a-checkbox>
          </a-checkbox-group>
        </a-form-item>
        <a-form-item field="authenticationMethods" label="客户端认证方式">
          <a-checkbox-group v-model="createClientForm.authenticationMethods">
            <a-checkbox value="client_secret_basic"
              >client_secret_basic</a-checkbox
            >
            <a-checkbox value="client_secret_post"
              >client_secret_post</a-checkbox
            >
            <a-checkbox value="none">none</a-checkbox>
          </a-checkbox-group>
        </a-form-item>
        <a-form-item field="scopes" label="OIDC Scope">
          <a-checkbox-group v-model="createClientForm.scopes">
            <a-checkbox
              :value="scope.name"
              v-for="(scope, index) in oidcScopes"
              :key="scope.name"
              >{{ scope.name }}</a-checkbox
            >
          </a-checkbox-group>
        </a-form-item>
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item
              field="authorizationCodeTimeToLive"
              label="授权码过期时间"
            >
              <a-input-group>
                <a-input-number
                  v-model="createClientForm.authorizationCodeTimeToLive"
                  :min="1"
                />
                <a-select
                  v-model="authorizationCodeTimeToLiveUnit"
                  :style="{ width: '120px' }"
                >
                  <a-option :value="1">分钟</a-option>
                  <a-option :value="60">小时</a-option>
                  <a-option :value="1440">天</a-option>
                </a-select>
              </a-input-group>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="accessTokenTimeToLive" label="访问令牌过期时间">
              <a-input-group>
                <a-input-number
                  v-model="createClientForm.accessTokenTimeToLive"
                  :min="1"
                />
                <a-select
                  v-model="accessTokenTimeToLiveUnit"
                  :style="{ width: '120px' }"
                >
                  <a-option :value="1">分钟</a-option>
                  <a-option :value="60">小时</a-option>
                  <a-option :value="1440">天</a-option>
                </a-select>
              </a-input-group>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="refreshTokenTimeToLive" label="刷新令牌过期时间">
          <a-input-group>
            <a-input-number
              v-model="createClientForm.refreshTokenTimeToLive"
              :min="1"
            />
            <a-select
              v-model="refreshTokenTimeToLiveUnit"
              :style="{ width: '120px' }"
            >
              <a-option :value="1">分钟</a-option>
              <a-option :value="60">小时</a-option>
              <a-option :value="1440">天</a-option>
            </a-select>
          </a-input-group>
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateClientForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>

  <a-modal
    v-model:visible="createClientSuccessModalVisible"
    hide-cancel
    :footer="false"
    :mask-closable="false"
    :width="660"
  >
    <template #title>
      <div>
        <icon-check-circle-fill
          style="color: rgb(var(--green-6)); margin-right: 4px"
        />
        创建客户端成功
      </div>
    </template>
    <div class="secret-modal">
      <copy-text :text="clientSecret" />
      <div class="info-text">客户端密钥仅显示一次，请及时保存。</div>
    </div>
  </a-modal>
</template>
