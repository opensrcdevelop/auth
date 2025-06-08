<script lang="ts">
import detailTs from "./index";
export default detailTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-header">
        <div>
          <span class="title">{{ resourceGroupName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="resourceGroupId" textColor="#86909c" />
          </div>
        </div>
        <a-button type="primary" @click="handleToCreateResource">
          创建资源
        </a-button>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="resource_group_info" title="资源组信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="resourceGroupInfoFrom"
              ref="resourceGroupInfoFromRef"
              :rules="resourceGroupInfoFormRules"
              layout="vertical"
              @submit-success="handleResourceGroupInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="资源组名称">
                    <a-input
                      v-model="resourceGroupInfoFrom.name"
                      placeholder="请输入资源组名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item
                    field="code"
                    label="资源组标识"
                    tooltip="创建后不可以修改"
                  >
                    <a-input v-model="resourceGroupInfoFrom.code" disabled />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="desc" label="资源组描述">
                <a-textarea
                  v-model="resourceGroupInfoFrom.desc"
                  placeholder="请输入资源组描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetResourceGroupInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="resource_list" title="资源列表">
          <div class="tab-container">
            <div class="info-title">资源列表</div>
            <a-input-search
              :style="{ width: '320px' }"
              placeholder="输入资源名称或标识进行搜索"
              allow-clear
              v-model="resourceSearchKeyword"
              @search="handleSearchResource"
              @clear="handleSearchResource"
              @keyup.enter.native="handleSearchResource"
            />
            <a-table
              class="resource-list"
              :data="resourceList"
              :pagination="resoueceListPagination.pagination"
              :bordered="false"
              :scroll="{ y: '100%' }"
              @page-size-change="resoueceListPagination.handlePageSizeChange"
              @page-change="resoueceListPagination.handlePageChange"
            >
              <template #columns>
                <a-table-column
                  title="资源名称"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToResourceDetail(record)"
                    >
                      {{ record.name }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column
                  title="资源标识"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span>
                      {{ record.code }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column
                  title="资源路径"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span>
                      {{ record.api }}
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
