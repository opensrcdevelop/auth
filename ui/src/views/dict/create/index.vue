<script lang="ts">
import indexTs from "./index";
export default indexTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建字典</div>
      <div class="info-title">字典信息</div>
      <a-form
        :model="createDictForm"
        :rules="createDictFormRules"
        ref="createDictFormRef"
        layout="vertical"
        @submit-success="handleCreateDictFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="字典名称">
              <a-input
                v-model="createDictForm.name"
                placeholder="请输入字典名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="字典标识">
              <a-input
                v-model="createDictForm.code"
                placeholder="请输入字典标识"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="字典描述">
          <a-textarea
            v-model="createDictForm.desc"
            placeholder="请输入字典描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateDictForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
