<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建身份源</div>
      <div class="info-title">身份源提供商</div>
      <div :class="identitySourceProvider.name ? 'provider-container' : ''">
        <a-row :gutter="24">
          <a-col :span="12">
            <a-input
              v-if="identitySourceProvider.name"
              :model-value="identitySourceProvider.name"
              disabled
            />
            <a-form
              v-else
              :model="identitySourceProvider"
              layout="vertical"
              ref="identitySourceProviderRef"
            >
              <a-form-item
                field="id"
                hide-label
                :rules="[{ required: true, message: '身份提供商未选择' }]"
              >
                <a-select
                  v-model="identitySourceProvider.id"
                  placeholder="请选择身份提供商"
                  allow-search
                  allow-clear
                >
                  <a-option
                    v-for="provider in providerList"
                    :key="provider.id"
                    :value="provider.id"
                  >
                    {{ provider.name }}
                  </a-option>
                </a-select>
              </a-form-item>
            </a-form>
          </a-col>
        </a-row>
      </div>
      <div class="info-title">身份源信息</div>
      <a-form
        :model="createIdentitySourceForm"
        layout="vertical"
        ref="createIdentitySourceFormRef"
        :rules="createIdentitySourceFormRules"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="身份源显示名称">
              <a-input
                v-model="createIdentitySourceForm.name"
                placeholder="请输入身份源显示名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="身份源标识">
              <a-input-group style="width: 100%">
                <a-input
                  v-model="createIdentitySourceForm.code"
                  placeholder="请输入身份源标识"
                />
                <a-button @click="generateRandomIdentitySourceCode"
                  >随机生成</a-button
                >
              </a-input-group>
            </a-form-item>
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="clientId" label="Client ID">
              <a-input
                v-model="createIdentitySourceForm.clientId"
                placeholder="请输入 Client ID"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="clientSecret" label="Client Secret">
              <a-input-password
                v-model="createIdentitySourceForm.clientSecret"
                placeholder="请输入 Client Secret"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item
              field="clientAuthenticationMethod"
              label="客户端认证方式"
            >
              <a-radio-group
                v-model="createIdentitySourceForm.clientAuthenticationMethod"
              >
                <a-radio value="client_secret_basic"
                  >client_secret_basic</a-radio
                >
                <a-radio value="client_secret_post">client_secret_post</a-radio>
              </a-radio-group>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="authorizationGrantType" label="授权类型">
              <a-radio-group
                v-model="createIdentitySourceForm.authorizationGrantType"
              >
                <a-radio value="authorization_code">authorization_code</a-radio>
              </a-radio-group>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item label="回调地址">
          <copy-text :text="callBackUrl" />
          <template #extra>
            <div>你需要将此链接配置到对应身份源的回调地址中</div>
          </template>
        </a-form-item>
        <a-form-item label="额外参数" field="additionalParams">
          <monaco-editor
            v-model="createIdentitySourceForm.additionalParams"
            language="json"
            :editorOption="{
              contextmenu: false,
            }"
            height="220px"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button
              type="primary"
              @click="handleCreateIdentitySourceFormSubmit"
              >创建</a-button
            >
            <a-button @click="handleResetCreateIdentitySourceForm"
              >重置</a-button
            >
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
