<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="identity-source-header">
      <div class="left">
        <div class="title">身份源列表</div>
        <div class="info">对身份源进行统一管理。</div>
      </div>
      <a-button type="primary">创建身份源</a-button>
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入身份源名称或标识进行搜索"
      allow-clear
      v-model="searchKeyword"
      @search="handleGetIdentitySourceList(1, 15)"
      @keyup.enter.native="handleGetIdentitySourceList(1, 15)"
      @clear="handleGetIdentitySourceList(1, 15)"
    />
    <div class="identity-source-list">
      <a-table
        :data="identitySourceList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="identitySourceListPagination.pagination"
        @page-change="identitySourceListPagination.handlePageChange"
        @page-size-change="identitySourceListPagination.handlePageSizeChange"
      >
        <template #columns>
          <a-table-column
            title="身份源显示名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-name"
                @click="handleToIdentitySourceDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="身份源标识"
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
            title="提供商"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-name"
                @click="handleToProviderDetail(record)"
              >
                {{ record.provider.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column title="状态">
            <template #cell="{ record }">
              <a-switch
                v-model="record.enabled"
                type="round"
                size="small"
                @change="handleUpdateIdentitySourceState(record)"
              />
            </template>
          </a-table-column>
          <a-table-column title="操作" :width="60">
            <template #cell="{ record }">
              <a-dropdown>
                <a-button type="text">
                  <template #icon>
                    <icon-more />
                  </template>
                </a-button>
                <template #content>
                  <a-doption
                    style="color: #e8353e"
                    @click="handleDeleteIdentitySource(record)"
                  >
                    <template #icon>
                      <icon-delete />
                    </template>
                    删除</a-doption
                  >
                </template>
              </a-dropdown>
            </template>
          </a-table-column>
        </template>
      </a-table>
    </div>
  </div>
</template>
