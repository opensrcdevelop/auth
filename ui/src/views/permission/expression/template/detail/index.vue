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
          <span class="title">{{ templateName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="templateId" textColor="#86909c" />
          </div>
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="template_info" title="限制条件模板信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
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
                  <a-button type="primary" @click="handleTemplateInfoFormSubmit"
                    >保存</a-button
                  >
                  <a-button @click="handleResetTemplateInfoForm">重置</a-button>
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="expression_list" title="关联限制条件">
          <div class="tab-container">
            <div class="info-title">关联限制条件</div>
            <a-table
              :data="expList"
              :bordered="false"
              :scroll="{ y: '100%' }"
              :pagination="false"
            >
              <template #columns>
                <a-table-column
                  title="条件名称"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToExpDetail(record)"
                    >
                      {{ record.name }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column
                  title="描述"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span>
                      {{ record.desc ? record.desc : "-" }}
                    </span>
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
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
