<script lang="ts">
import resourceGroupTs from "./index";
export default resourceGroupTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="resource-group-header">
      <div class="left">
        <div class="title">资源组管理</div>
        <div class="info">进行资源组的增删改查。</div>
      </div>
      <a-button type="primary" @click="handleToCreateResourceGroup"
        >创建资源组</a-button
      >
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入资源组名称或标识进行搜索"
      allow-clear
      v-model="resourceGroupSearchKeyword"
      @search="handleGetResourceGroupList(1, 15)"
      @keyup.enter.native="handleGetResourceGroupList(1, 15)"
      @clear="handleGetResourceGroupList(1, 15)"
    />
    <div class="resource-group-list">
      <a-table
        :data="resourceGroupList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="resourceGroupListPagination"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
      >
        <template #columns>
          <a-table-column
            title="资源组名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-resourcegroupname"
                @click="handleToResourceGroupDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="资源组标识"
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
                    @click="handleDeleteResourceGroup(record)"
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
