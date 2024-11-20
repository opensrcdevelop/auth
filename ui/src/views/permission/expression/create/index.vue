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
      <div class="create-tile">创建限制条件</div>
      <div class="info-title">限制条件信息</div>
      <a-form
        :model="createPermissionExpInfoForm"
        ref="createPermissionExpInfoFormRef"
        :rules="createPermissionExpInfoFormRules"
        layout="vertical"
        @submit-success="handleCreatePermissionExpInfoFormSubmit"
      >
        <a-form-item field="name" label="限制条件名称">
          <a-input
            v-model="createPermissionExpInfoForm.name"
            placeholder="请输入限制条件名称"
          />
        </a-form-item>
        <a-form-item field="expression" label="SpringEL 表达式">
          <a-textarea
            v-model="createPermissionExpInfoForm.expression"
            placeholder="请输入 SpringEL 表达式"
          />
        </a-form-item>
        <a-form-item field="desc" label="限制条件描述">
          <a-textarea
            v-model="createPermissionExpInfoForm.desc"
            placeholder="请输入限制条件描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreatePermissionExpInfoForm"
              >重置</a-button
            >
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
