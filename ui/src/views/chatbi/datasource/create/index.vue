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
      <div class="create-tile">创建数据源</div>
      <div class="info-title">数据源信息</div>
      <a-form
        :model="createDataSourceForm"
        layout="vertical"
        ref="createDataSourceFormRef"
        :rules="createDataSourceFormRules"
        @submit-success="handleCreateDataSourceFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="数据源名称">
              <a-input
                v-model="createDataSourceForm.name"
                placeholder="请输入数据源名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="type" label="数据源类型">
              <a-select
                v-model="createDataSourceForm.type"
                placeholder="请选择数据源类型"
              >
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
              <a-input
                v-model="createDataSourceForm.database"
                placeholder="请输入数据库"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="schema" label="模式">
              <a-input
                v-model="createDataSourceForm.schema"
                placeholder="请输入模式"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="host" label="主机地址">
              <a-input
                v-model="createDataSourceForm.host"
                placeholder="请输入主机地址"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="port" label="端口号">
              <a-input-number
                v-model="createDataSourceForm.port"
                :min="0"
                hide-button
                placeholder="请输入端口号"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="username" label="用户名">
              <a-input
                v-model="createDataSourceForm.username"
                placeholder="请输入用户名"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="password" label="密码">
              <a-input-password
                v-model="createDataSourceForm.password"
                placeholder="请输入密码"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="jdbcParams" label="JDBC 参数">
          <a-input
            v-model="createDataSourceForm.jdbcParams"
            placeholder="请输入 JDBC 参数"
          />
        </a-form-item>
        <a-form-item field="desc" label="数据源描述">
          <a-textarea
            v-model="createDataSourceForm.desc"
            placeholder="请输入数据源描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" @click="hanleTestConn">测试连接</a-button>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateDataSourceForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
