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
          <span class="title">{{ modelProviderName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="modelProviderId" textColor="#86909c" />
          </div>
        </div>
      </div>
    </page-header>

    <a-tabs :active-key="activeTab">
      <a-tab-pane key="model_provider_info" title="模型提供商信息">
        <div class="tab-container">
          <div class="info-title">基本信息</div>
          <a-form
            :model="modelProviderInfoForm"
            layout="vertical"
            ref="modelProviderInfoFormRef"
            :rules="modelProviderInfoFormRules"
            @submit-success="handleModelProviderInfoFormSubmit"
          >
            <a-row :gutter="24">
              <a-col :span="12">
                <a-form-item field="name" label="模型提供商名称">
                  <a-input
                    v-model="modelProviderInfoForm.name"
                    placeholder="请输入模型提供商名称"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="type" label="模型提供商类型">
                  <a-select v-model="modelProviderInfoForm.type" disabled>
                    <a-option
                      v-for="item of modelProviderTypeList"
                      :value="item.value"
                      :label="item.label"
                    />
                  </a-select>
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="baseUrl" label="API 接入地址">
                  <a-input
                    v-model="modelProviderInfoForm.baseUrl"
                    placeholder="请输入 API 接入地址"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="apiKey" label="API Key">
                  <a-input-password
                    v-model="modelProviderInfoForm.apiKey"
                    placeholder="请输入 API Key"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="temperature" label="温度参数">
                  <a-input-number
                    v-model="modelProviderInfoForm.temperature"
                    :min="0"
                    hide-button
                    placeholder="请输入温度参数"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="maxTokens" label="最大输出 token 长度">
                  <a-input-number
                    v-model="modelProviderInfoForm.maxTokens"
                    :min="50"
                    hide-button
                    placeholder="请输入最大输出 token 长度"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="defaultModel" label="默认模型">
                  <a-select v-model="modelProviderInfoForm.defaultModel">
                    <a-option
                      v-for="item of optionalModelList"
                      :value="item.name"
                      :label="item.name"
                    />
                  </a-select>
                </a-form-item>
              </a-col>
            </a-row>
            <a-form-item hide-label>
              <a-space>
                <a-button type="primary" html-type="submit">保存</a-button>
                <a-button @click="handleResetModelProviderInfoForm"
                  >重置</a-button
                >
              </a-space>
            </a-form-item>
          </a-form>
          <div class="info-title">可选模型列表</div>
          <div class="add-model-btn">
            <a-button
              type="text"
              size="mini"
              @click="() => (addOptionalModelModalVisible = true)"
            >
              <template #icon>
                <icon-plus-circle />
              </template>
              <template #default>添加模型</template>
            </a-button>
          </div>
          <a-table
            :data="optionalModelList"
            :bordered="false"
            :pagination="false"
            :draggable="{ type: 'handle', width: 40 }"
            @change="handleOptionalModelListChange"
          >
            <template #columns>
              <a-table-column title="模型名称">
                <template #cell="{ record }">
                  <span>{{ record.name }}</span>
                </template>
              </a-table-column>
              <a-table-column title="已使用输入 token 数">
                <template #cell="{ record }">
                  <span>{{ record.usedReqTokens.toLocaleString() }}</span>
                </template>
              </a-table-column>
              <a-table-column title="已使用输出 token 数">
                <template #cell="{ record }">
                  <span>{{ record.usedRepTokens.toLocaleString() }}</span>
                </template>
              </a-table-column>
              <a-table-column title="操作" :width="80">
                <template #cell="{ record }">
                  <a-button
                    type="text"
                    status="danger"
                    size="mini"
                    v-if="record.name !== modelProviderInfoForm.defaultModel"
                    @click="handleRemoveOptionalModel(record)"
                  >
                    <template #icon>
                      <icon-delete />
                    </template>
                    <template #default>删除</template>
                  </a-button>
                </template>
              </a-table-column>
            </template>
          </a-table>
        </div>
      </a-tab-pane>
    </a-tabs>

    <a-modal
      v-model:visible="addOptionalModelModalVisible"
      @ok="handleAddOptionalModelFormSubmit"
      @cancel="handleCloseAddOptionalModelModal"
    >
      <template #title> 添加模型 </template>
      <div>
        <a-form
          :model="addOptionalModelForm"
          ref="addOptionalModelFormRef"
          :rules="addOptionalModelFormRules"
        >
          <a-form-item field="name" hide-label>
            <a-input
              v-model="addOptionalModelForm.name"
              placeholder="请输入模型名称"
            />
          </a-form-item>
        </a-form>
      </div>
    </a-modal>
  </div>
</template>
