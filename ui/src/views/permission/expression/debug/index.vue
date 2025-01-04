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
      <div class="debug-tile">调试限制条件</div>
      <div class="info-title">限制条件</div>
      <a-form :model="permissionExp" layout="vertical">
        <a-form-item field="name" label="限制条件名称">
          <a-input readonly v-model="permissionExp.name" />
        </a-form-item>
        <a-form-item field="expression" label="SpringEL 表达式">
          <a-textarea readonly auto-size v-model="permissionExp.expression" />
        </a-form-item>
      </a-form>
      <div class="info-title">执行上下文</div>
      <a-form
        ref="debugPermissionExpFormRef"
        :model="debugPermissionExpForm"
        :rules="debugPermissionExpFormRules"
        layout="vertical"
        @submit-success="handleDebugPermissionExpFormSubmit"
      >
        <a-form-item field="context" label="上下文">
          <monaco-editor
            v-model="debugPermissionExpForm.context"
            language="json"
            :editorOption="{
              contextmenu: false,
            }"
            height="280px"
          />
          <template #extra>
            <div>
              上下文为 JSON 对象格式，请确保输入的上下文符合 JSON 格式要求。
            </div>
          </template>
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">调试运行</a-button>
            <a-button @click="handleResetDebugPermissionExpForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
