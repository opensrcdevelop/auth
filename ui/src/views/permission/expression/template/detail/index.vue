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
        <a-button type="primary" @click="handleOpenDebugDrawer">
          调试运行
        </a-button>
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

    <a-drawer
      :width="540"
      :visible="debugDrawerVisible"
      ok-text="调试运行"
      :ok-loading="debugFormSubmitLoading"
      @cancel="handleCloseDebugDrawer"
      @ok="handleDebugFormSubmit"
    >
      <template #title>调试运行限制条件模板</template>
      <a-form
        :model="debugForm"
        :rules="debugFormRules"
        ref="debugFormRef"
        layout="vertical"
      >
        <ParamInput
          ref="debugParamRef"
          :configs="debugParamConfigs"
          v-model="debugForm.templateParams"
        />
        <a-form-item field="context" label="上下文">
          <monaco-editor
            v-model="debugForm.context"
            language="json"
            height="280px"
            :editorOption="{
              contextmenu: false,
            }"
          />
          <template #extra>
            <div>
              上下文为 JSON 对象格式，请确保输入的上下文符合 JSON 格式要求
            </div>
          </template>
        </a-form-item>
      </a-form>
    </a-drawer>

    <a-modal
      :visible="debugResultModalVisible"
      :footer="false"
      :mask-closable="false"
      @cancel="debugResultModalVisible = false"
    >
      <template #title>调试运行结果</template>
      <a-descriptions :column="1" bordered>
        <a-descriptions-item label="状态">
          {{ debugResult.success ? "成功" : "失败" }}
        </a-descriptions-item>
        <a-descriptions-item label="结果" v-if="debugResult.success">
          <a-tag v-if="debugResult.execResult" color="#00b42a">
            <template #icon>
              <icon-check-circle-fill style="color: #fff" />
            </template>
            允许
          </a-tag>
          <a-tag v-else color="#f53f3f">
            <template #icon>
              <icon-minus-circle-fill style="color: #fff" />
            </template>
            拒绝
          </a-tag>
        </a-descriptions-item>
        <a-descriptions-item label="错误" v-else>
          {{ debugResult.execResult }}
        </a-descriptions-item>
      </a-descriptions>
    </a-modal>
  </div>
</template>
