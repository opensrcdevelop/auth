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
      <div class="detail-header">
        <div>
          <span class="title">{{ dictDataLabel }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="dictDataId" textColor="#86909c" />
          </div>
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="dict_data_info" title="字典数据信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="dictDataInfoForm"
              ref="dictDataInfoFormRef"
              :rules="dictDataInfoFormRules"
              layout="vertical"
              @submit-success="handleDictDataInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="label" label="数据标签">
                    <a-input
                      v-model="dictDataInfoForm.label"
                      placeholder="请输入数据标签"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="value" label="数据值">
                    <a-input
                      v-model="dictDataInfoForm.value"
                      placeholder="请输入数据值"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="displaySeq" label="显示顺序">
                    <a-input-number
                      v-model="dictDataInfoForm.displaySeq"
                      placeholder="请输入显示顺序"
                      :min="1"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="enable" label="状态">
                    <a-select v-model="dictDataInfoForm.enable">
                      <a-option :value="true">启用</a-option>
                      <a-option :value="false">禁用</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetDictDataInfoForm">重置</a-button>
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>
  </div>
</template>
