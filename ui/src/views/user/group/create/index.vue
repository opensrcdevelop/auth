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
      <div class="create-tile">创建用户组</div>
      <div class="info-title">用户组信息</div>
      <a-form
        :model="createUserGroupForm"
        ref="createUserGroupFormRef"
        :rules="createUserGroupFormRules"
        layout="vertical"
        @submit-success="handleCreateUserGroupFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="用户组名称">
              <a-input
                v-model="createUserGroupForm.name"
                placeholder="请输入用户组名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="用户组标识">
              <a-input
                v-model="createUserGroupForm.code"
                placeholder="请输入用户组标识"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="用户组描述">
          <a-textarea
            v-model="createUserGroupForm.desc"
            placeholder="请输入用户组描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateUserGroupForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
