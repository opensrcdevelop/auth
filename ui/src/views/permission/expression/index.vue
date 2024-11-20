<script lang="ts">
import expressionTs from "./index";
export default expressionTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="expression-header">
      <div class="left">
        <div class="title">限制条件</div>
        <div class="info">
          限制条件用于控制用户对资源权限的访问，如限制客户端 IP、请求时间等。
        </div>
      </div>
      <a-button type="primary" @click="handleToCreatePermssionExp"
        >创建限制条件</a-button
      >
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入条件名称或表达式进行搜索"
      allow-clear
      v-model="permissionExpSearchKeyword"
      @search="handleGetPermissionExpList(1, 15)"
      @keyup.enter.native="handleGetPermissionExpList(1, 15)"
      @clear="handleGetPermissionExpList(1, 15)"
    />
    <div class="expression-list">
      <a-table
        :data="permissionExpList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="permissionExpListPagination"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
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
                class="table-column-expressionname"
                @click="handleToPermissionExpDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="SpringEL 表达式"
            ellipsis
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span>
                {{ record.expression }}
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
                    @click="handleDeletePermissionExp(record)"
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
