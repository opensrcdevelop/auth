<script lang="ts">
import detailTs from "./index";
export default detailTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-header">
        <div>
          <span class="title">{{ tenantName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="tenantId" textColor="#86909c" />
          </div>
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="tanant_info" title="租户信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="tenantInfoForm"
              ref="tenantInfoFormRef"
              :rules="tenantInfoFormFormRules"
              layout="vertical"
              @submit-success="handleTenantInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="6">
                  <a-form-item field="name" label="租户名称">
                    <a-input
                      v-model="tenantInfoForm.name"
                      placeholder="请输入租户名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item
                    field="name"
                    label="租户标识"
                    tooltip="创建后不可以修改"
                  >
                    <a-input v-model="tenantInfoForm.code" disabled />
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item field="enabled" label="租户状态">
                    <a-select v-model="tenantInfoForm.enabled">
                      <a-option :value="true">启用</a-option>
                      <a-option :value="false">禁用</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="6">
                  <a-form-item field="createTime" label="创建时间">
                    <a-input readonly v-model="tenantInfoForm.createTime" />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="desc" label="租户描述">
                <a-textarea
                  v-model="tenantInfoForm.desc"
                  placeholder="请输入租户描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetTenantInfoForm">重置</a-button>
                </a-space>
              </a-form-item>
            </a-form>
            <div class="info-title">端点信息</div>
            <a-descriptions :column="2" :align="{ label: 'right' }">
              <a-descriptions-item label="Issuer">{{
                endpointInfo.issuer
              }}</a-descriptions-item>
              <a-descriptions-item label="控制台 URL">
                <a-link>
                  <a :href="endpointInfo.consoleUrl" target="_blank">{{
                    endpointInfo.consoleUrl
                  }}</a>
                  <icon-launch style="margin-left: 4px" /> </a-link
              ></a-descriptions-item>
            </a-descriptions>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>
  </div>
</template>
