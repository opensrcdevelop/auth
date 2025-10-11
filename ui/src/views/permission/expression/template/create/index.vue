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
      <div class="create-tile">创建限制条件模板</div>
      <div class="info-title">模板信息</div>
      <a-form
        :model="templateInfoForm"
        :rules="templateInfoFormRules"
        ref="templateInfoFormRef"
        layout="vertical"
      >
        <a-form-item field="name" label="模板名称">
          <a-input
            v-model="templateInfoForm.name"
            placeholder="请输入模板名称"
          />
        </a-form-item>
        <a-form-item field="desc" label="模板描述">
          <a-textarea
            v-model="templateInfoForm.desc"
            placeholder="请输入模板描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item field="expression" label="JEXL 表达式">
          <monaco-editor
            v-model="templateInfoForm.expression"
            language="jexl"
            height="280px"
            :editorOption="{
              contextmenu: false,
              theme: 'jexl-vs-theme',
            }"
          />
        </a-form-item>
        <a-form-item
          :field="`paramConfigs[${index}]`"
          :label="`模板参数 - ${index + 1}`"
          v-for="(item, index) in templateInfoForm.paramConfigs"
          :key="index"
        >
          <ParamSelect
            :ref="
              (el) => {
                if (el) paramConfigRefs[index] = el;
              }
            "
            v-model="templateInfoForm.paramConfigs[index]"
            @remove="handleRemoveParamConfig(index)"
          />
        </a-form-item>
        <div class="add-param-config">
          <a-button type="text" @click="handleParamConfigModalOpen">
            <template #icon>
              <icon-plus />
            </template>
            <template #default> 添加参数 </template>
          </a-button>
        </div>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" @click="handleTemplateInfoFormSubmit">创建</a-button>
            <a-button @click="handleResetTemplateInfoForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>

    <a-modal
      v-model:visible="addParamConfigModalVisible"
      @ok="handleParamConfigModalConfirm"
      @cancel="handleParamConfigModalClose"
      :mask-closable="false"
      :hide-cancel="true"
    >
      <template #title> 选择参数类型 </template>
      <a-select v-model="selectedParamType" placeholder="请选择参数类型">
        <a-option v-for="(item, index) in parmaTypes" :value="item.value">{{
          item.label
        }}</a-option>
      </a-select>
    </a-modal>
  </div>
</template>
