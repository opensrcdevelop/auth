<script lang="ts">
import createTs from "./index";

export default createTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
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
          <a-col :span="12">
            <a-form-item field="type" label="用户组类型">
              <a-radio-group
                v-model="createUserGroupForm.type"
                @change="handleUserGroupTypeChange"
              >
                <a-radio value="STATIC">静态用户组</a-radio>
                <a-radio value="DYNAMIC">动态用户组</a-radio>
              </a-radio-group>
            </a-form-item>
          </a-col>
          <a-col :span="12">
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
          </a-col>
        </a-row>
        <div class="info-title">用户组规则</div>
        <UserGroupConditions
          v-if="createUserGroupForm.type === 'DYNAMIC'"
          :conditions="createUserGroupForm.conditions"
          ref="userGroupConditionsRef"
        />
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" @click="handleCreateUserGroupFormSubmit"
              >创建</a-button
            >
            <a-button @click="handleResetCreateUserGroupForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
