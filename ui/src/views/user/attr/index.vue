<script lang="ts">
import userAttrTs from "./index";
export default userAttrTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="user-column-header">
      <div class="left">
        <div class="title">字段管理</div>
        <div class="info">进行用户字段的统一管理，自定义用户字段。</div>
      </div>
      <a-button type="primary" @click="handleToCreateUserColumn"
        >创建字段</a-button
      >
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入字段名称或 key 进行搜索"
      allow-clear
      v-model="userColumnSearchKeyword"
      @search="handleGetUserColumnList(1, 15)"
      @keyup.enter.native="handleGetUserColumnList(1, 15)"
      @clear="handleGetUserColumnList(1, 15)"
    />
    <div class="user-column-list">
      <a-table
        :data="userColumnList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="userColumnListPagination.pagination"
        @page-change="userColumnListPagination.handlePageChange"
        @page-size-change="userColumnListPagination.handlePageSizeChange"
      >
        <template #columns>
          <a-table-column
            title="字段名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-columnname"
                @click="handleToUserColumnDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="字段 key"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span>
                {{ record.key }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="表单类型"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span v-if="record.dataType === 'STRING'"> 字符串 </span>
              <span v-if="record.dataType === 'BOOLEAN'"> 布尔值 </span>
              <span v-if="record.dataType === 'NUMBER'"> 数字 </span>
              <span v-if="record.dataType === 'DATETIME'"> 日期时间 </span>
              <span v-if="record.dataType === 'DATE'"> 日期 </span>
              <span v-if="record.dataType === 'DICT'"> 字典 </span>
            </template>
          </a-table-column>
          <a-table-column
            title="字段类型"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span>{{ record.extFlg ? "扩展字段" : "基础字段" }}</span>
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
                    @click="handleDeleteUserColumn(record)"
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
