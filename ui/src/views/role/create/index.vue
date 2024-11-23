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
      <div class="create-tile">创建角色</div>
      <div class="info-title">角色信息</div>
      <a-form
        :model="createRoleForm"
        ref="createRoleFormRef"
        :rules="createRoleFormRules"
        layout="vertical"
        @submit-success="handleCreateRoleFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="角色名称">
              <a-input
                v-model="createRoleForm.name"
                placeholder="请输入角色名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="角色标识">
              <a-input
                v-model="createRoleForm.code"
                placeholder="请输入角色标识"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="角色描述">
          <a-textarea
            v-model="createRoleForm.desc"
            placeholder="请输入角色描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateRoleForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
