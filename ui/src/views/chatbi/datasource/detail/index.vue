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
          <span class="title">{{ dataSourceName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="dataSourceId" textColor="#86909c" />
          </div>
        </div>
        <a-button type="primary" @click="hanleTestConn()">测试连接</a-button>
      </div>
    </page-header>
    <a-tabs :active-key="activeTab" @change="handleTabChange">
      <a-tab-pane key="data_source_info" title="数据源信息">
        <div class="tab-container">
          <div class="info-title">基本信息</div>
          <a-form
            :model="dataSourceInfoForm"
            layout="vertical"
            ref="dataSourceInfoFormRef"
            :rules="dataSourceInfoFormRules"
            @submit-success="handleDataSourceInfoFormSubmit"
          >
            <a-row :gutter="24">
              <a-col :span="12">
                <a-form-item field="name" label="数据源名称">
                  <a-input
                    v-model="dataSourceInfoForm.name"
                    placeholder="请输入数据源名称"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="type" label="数据源类型">
                  <a-select v-model="dataSourceInfoForm.type" disabled>
                    <a-option
                      v-for="item of dataSourceTypeList"
                      :value="item.value"
                      :label="item.label"
                    />
                  </a-select>
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="database" label="数据库">
                  <a-input disabled v-model="dataSourceInfoForm.database" />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="schema" label="模式">
                  <a-input disabled v-model="dataSourceInfoForm.schema" />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="host" label="主机地址">
                  <a-input
                    v-model="dataSourceInfoForm.host"
                    placeholder="请输入主机地址"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="port" label="端口号">
                  <a-input-number
                    v-model="dataSourceInfoForm.port"
                    :min="0"
                    hide-button
                    placeholder="请输入端口号"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="username" label="用户名">
                  <a-input
                    v-model="dataSourceInfoForm.username"
                    placeholder="请输入用户名"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="password" label="密码">
                  <a-input-password
                    v-model="dataSourceInfoForm.password"
                    placeholder="请输入密码"
                  />
                </a-form-item>
              </a-col>
            </a-row>
            <a-form-item field="jdbcParams" label="JDBC 参数">
              <a-input
                v-model="dataSourceInfoForm.jdbcParams"
                placeholder="请输入 JDBC 参数"
              />
            </a-form-item>
            <a-form-item field="desc" label="数据源描述">
              <a-textarea
                v-model="dataSourceInfoForm.desc"
                placeholder="请输入数据源描述"
                :auto-size="{
                  minRows: 3,
                  maxRows: 5,
                }"
              />
            </a-form-item>
            <a-form-item hide-label>
              <a-space>
                <a-button type="primary" html-type="submit">保存</a-button>
                <a-button @click="handleResetDataSourceInfoForm">重置</a-button>
              </a-space>
            </a-form-item>
          </a-form>
        </div>
      </a-tab-pane>
      <a-tab-pane key="table_list" title="数据表列表">
        <div class="tab-container"></div>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
