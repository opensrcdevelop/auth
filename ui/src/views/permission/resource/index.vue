<script lang="ts">
import resourceTs from "./index";
export default resourceTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="resource-header">
      <div class="left">
        <div class="title">资源权限</div>
        <div class="info">
          进行资源的增删改查，定义权限，将资源与权限授权给不同的主体。
        </div>
      </div>
      <a-space>
        <a-button @click="handleAuthorize">授权</a-button>
        <a-button type="primary" @click="handleToCreateResource"
          >创建资源</a-button
        >
      </a-space>
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入资源名或标识进行搜索"
      allow-clear
      v-model="resourceSearchKeyword"
      @search="handleGetResourceList(1, 15)"
      @keyup.enter.native="handleGetResourceList(1, 15)"
      @clear="handleGetResourceList(1, 15)"
    />
    <div class="resource-list">
      <a-table
        :data="resourceList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="resourceListPagination"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
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
                class="table-column-resourcename"
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
          <a-table-column
            title="资源组名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span>
                {{ record.resourceGroup.name }}
              </span>
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
                    @click="handleDeleteResource(record)"
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

    <authorize :visible="authorizeVisible" @close="authorizeVisible = false" />
  </div>
</template>
