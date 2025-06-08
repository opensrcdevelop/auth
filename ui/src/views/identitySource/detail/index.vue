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
          <span class="title">{{ registrationName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="registrationId" textColor="#86909c" />
          </div>
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="registration_info" title="身份源信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="registrationInfoForm"
              layout="vertical"
              ref="registrationInfoFormRef"
              :rules="registrationInfoFormRules"
              @submit-success="handleRegistrationInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="身份源显示名称">
                    <a-input
                      v-model="registrationInfoForm.name"
                      placeholder="请输入身份源显示名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="code" label="身份源标识">
                    <a-input-group style="width: 100%">
                      <a-input
                        v-model="registrationInfoForm.code"
                        placeholder="请输入身份源标识"
                      />
                      <a-button @click="generateRandomRegistraionCode"
                        >随机生成</a-button
                      >
                    </a-input-group>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="clientId" label="Client ID">
                    <a-input
                      v-model="registrationInfoForm.clientId"
                      placeholder="请输入 Client ID"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="clientSecret" label="Client Secret">
                    <a-input-password
                      v-model="registrationInfoForm.clientSecret"
                      placeholder="请输入 Client Secret"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item
                    field="clientAuthenticationMethod"
                    label="客户端认证方式"
                  >
                    <a-radio-group
                      v-model="registrationInfoForm.clientAuthenticationMethod"
                    >
                      <a-radio value="client_secret_basic"
                        >client_secret_basic</a-radio
                      >
                      <a-radio value="client_secret_post"
                        >client_secret_post</a-radio
                      >
                    </a-radio-group>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="authorizationGrantType" label="授权类型">
                    <a-radio-group
                      v-model="registrationInfoForm.authorizationGrantType"
                    >
                      <a-radio value="authorization_code"
                        >authorization_code</a-radio
                      >
                    </a-radio-group>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item label="回调地址">
                <copy-text :text="callBackUrl" />
                <template #extra>
                  <div>你需要将此链接配置到对应身份源的回调地址中</div>
                </template>
              </a-form-item>
              <a-form-item label="额外参数" field="additionalParams">
                <monaco-editor
                  v-model="registrationInfoForm.additionalParams"
                  language="json"
                  :editorOption="{
                    contextmenu: false,
                  }"
                  height="220px"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetRegistrationInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="user_bindings" title="关联用户">
          <div class="tab-container">
            <div class="info-title">关联用户</div>
            <a-input-search
              :style="{ width: '320px', marginBottom: '16px' }"
              placeholder="输入用户名进行搜索"
              allow-clear
              v-model="userBindingSearchKeyword"
              @search="handleSearchUserBinding"
              @clear="handleSearchUserBinding"
              @keyup.enter.native="handleSearchUserBinding"
            />
            <a-table
              :data="userBindingList"
              :bordered="false"
              :scroll="{ y: '100%' }"
              :pagination="userBindingListPagination.pagination"
              @page-size-change="userBindingListPagination.handlePageSizeChange"
              @page-change="userBindingListPagination.handlePageChange"
            >
              <template #columns>
                <a-table-column
                  title="用户名"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToUserDetail(record.userId)"
                    >
                      {{ record.username }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column
                  title="唯一标识"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span>
                      {{ record.uniqueId }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column
                  title="绑定时间"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span>
                      {{ record.bindingTime }}
                    </span>
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>
  </div>
</template>
