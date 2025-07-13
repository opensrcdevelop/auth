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
      <div class="create-tile">创建限制条件</div>
      <div class="info-title">限制条件信息</div>
      <a-form
        :model="createPermissionExpInfoForm"
        ref="createPermissionExpInfoFormRef"
        :rules="createPermissionExpInfoFormRules"
        layout="vertical"
      >
        <a-form-item field="name" label="限制条件名称">
          <a-input
            v-model="createPermissionExpInfoForm.name"
            placeholder="请输入限制条件名称"
          />
        </a-form-item>
        <a-form-item field="templateId" label="限制条件模板">
          <a-select
            placeholder="请选择限制条件模板"
            allow-search
            allow-clear
            v-model="createPermissionExpInfoForm.templateId"
            @change="handleTemplateSelectChange"
          >
            <a-option v-for="item in templateList" :value="item.id">
              {{ item.name }}
            </a-option>
          </a-select>
        </a-form-item>
        <div v-if="templateParamConfigs.length > 0">
          <ParamInput
            ref="templateParamsRef"
            :configs="templateParamConfigs"
            v-model="createPermissionExpInfoForm.templateParams"
          />
        </div>
        <a-form-item
          v-if="!createPermissionExpInfoForm.templateId"
          field="expression"
          label="JEXL 表达式"
        >
          <monaco-editor
            v-model="createPermissionExpInfoForm.expression"
            language="jexl"
            :editorOption="{
              contextmenu: false,
            }"
            height="280px"
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
            <a-button type="primary" @click="handleCreatePermissionExpInfoFormSubmit">创建</a-button>
            <a-button @click="handleResetCreatePermissionExpInfoForm"
              >重置</a-button
            >
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
