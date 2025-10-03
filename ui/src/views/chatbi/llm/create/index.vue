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
      <div class="create-tile">创建模型提供商</div>
      <div class="info-title">模型提供商信息</div>
      <a-form
        :model="createModelProviderForm"
        layout="vertical"
        ref="createModelProviderFormRef"
        :rules="createModelProviderFormRules"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="模型提供商名称">
              <a-input
                v-model="createModelProviderForm.name"
                placeholder="请输入模型提供商名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="type" label="模型提供商类型">
              <a-select v-model="createModelProviderForm.type">
                <a-option
                  v-for="item of modelProviderTypeList"
                  :value="item.value"
                  :label="item.label"
                />
              </a-select>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="baseUrl" label="API 接入地址">
              <a-input
                v-model="createModelProviderForm.baseUrl"
                placeholder="请输入 API 接入地址"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="apiKey" label="API Key">
              <a-input
                v-model="createModelProviderForm.apiKey"
                placeholder="请输入 API Key"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="temperature" label="温度参数">
              <a-input-number
                v-model="createModelProviderForm.temperature"
                :min="0"
                hide-button
                placeholder="请输入温度参数"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="maxTokens" label="最大输出 token 长度">
              <a-input-number
                v-model="createModelProviderForm.maxTokens"
                :min="50"
                hide-button
                placeholder="请输入最大输出 token 长度"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="defaultModel" label="默认模型">
              <a-select
                v-model="createModelProviderForm.defaultModel"
                allow-create
              >
                <a-option
                  v-for="item of createModelProviderForm.optionalModels"
                  :value="item.name"
                  :label="item.name"
                />
              </a-select>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">保存</a-button>
            <a-button>重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
