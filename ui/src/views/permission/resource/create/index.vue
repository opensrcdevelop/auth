<script lang="ts">
import createTs from "./index";
export default createTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建资源</div>
      <div class="info-title">资源信息</div>
      <a-form
        :model="createResourceForm"
        ref="createResourceFormRef"
        :rules="createResourceFormRules"
        layout="vertical"
        @submit-success="handleCreateResourceFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="资源名称">
              <a-input
                v-model="createResourceForm.name"
                placeholder="请输入资源名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="资源标识">
              <a-input
                v-model="createResourceForm.code"
                placeholder="请输入资源标识"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="showSelectResourceGroup ? 12 : 24">
            <a-form-item field="api" label="资源路径">
              <a-input
                v-model="createResourceForm.api"
                placeholder="请输入资源路径（URL）"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12" v-if="showSelectResourceGroup">
            <a-form-item field="resourceGroupId" label="资源组">
              <a-select
                placeholder="请选择资源组"
                allow-clear
                allow-search
                v-model:model-value="createResourceForm.resourceGroupId"
                v-model:input-value="resourceGroupSearchKeyword"
                @search="handleSearchResourceGroup"
                @clear="handleSearchResourceGroup"
                :filter-option="false"
                @dropdown-reach-bottom="loadMoreResourceGroup"
              >
                <a-option
                  v-for="(resourceGroup, index) in resourceGroupList"
                  :key="resourceGroup.id"
                  :value="resourceGroup.id"
                >
                  {{ resourceGroup.name }} - {{ resourceGroup.code }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="desc" label="资源描述">
          <a-textarea
            v-model="createResourceForm.desc"
            placeholder="请输入资源描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateResourceForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
