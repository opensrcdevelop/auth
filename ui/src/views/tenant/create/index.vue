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
      <div class="create-tile">创建租户</div>
      <div class="info-title">租户信息</div>
      <a-form
        :model="createTenantInfoForm"
        ref="createTenantInfoFormRef"
        :rules="createTenantInfoFormRules"
        layout="vertical"
        @submit-success="handleCreateTenantInfoFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="租户名称">
              <a-input
                v-model="createTenantInfoForm.name"
                placeholder="请输入租户名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item
              field="code"
              label="租户标识"
              tooltip="创建后不可以修改"
            >
              <a-input-group style="width: 100%">
                <a-input
                  v-model="createTenantInfoForm.code"
                  placeholder="请输入租户标识"
                />
                <a-button @click="handleGenerateTenantCode"
                  >生成租户标识</a-button
                >
              </a-input-group>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="租户描述">
          <a-textarea
            v-model="createTenantInfoForm.desc"
            placeholder="请输入租户描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateTenantInfoForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
