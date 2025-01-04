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
      <div class="create-tile">创建字典数据</div>
      <div class="info-title">字典数据信息</div>
      <a-form
        :model="createDictDataForm"
        ref="createDictDataFormRef"
        :rules="createDictDataFormRules"
        layout="vertical"
        @submit-success="handleCreateDictDataFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="label" label="数据标签">
              <a-input
                v-model="createDictDataForm.label"
                placeholder="请输入数据标签"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="value" label="数据值">
              <a-input
                v-model="createDictDataForm.value"
                placeholder="请输入数据值"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="displaySeq" label="显示顺序">
              <a-input-number
                v-model="createDictDataForm.displaySeq"
                placeholder="请输入显示顺序"
                :min="1"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="enable" label="状态">
              <a-select v-model="createDictDataForm.enable">
                <a-option :value="true">启用</a-option>
                <a-option :value="false">禁用</a-option>
              </a-select>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateDictDataForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
