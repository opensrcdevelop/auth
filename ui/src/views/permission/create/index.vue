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
      <div class="create-tile">创建权限</div>
      <div class="info-title">权限信息</div>
      <a-form
        :model="createPermissionForm"
        ref="createPermissionFormRef"
        :rules="createPermissionFormRules"
        layout="vertical"
        @submit-success="handleCreatePermissionFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="权限名称">
              <a-input
                v-model="createPermissionForm.name"
                placeholder="请输入权限名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="权限标识">
              <a-input
                v-model="createPermissionForm.code"
                placeholder="请输入权限标识"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="权限描述">
          <a-textarea
            v-model="createPermissionForm.desc"
            placeholder="请输入权限描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreatePermissionForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
