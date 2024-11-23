<script lang="ts">
import userGroupTs from "./index";
export default userGroupTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="user-group-header">
      <div class="left">
        <div class="title">用户组管理</div>
        <div class="info">进行一组用户的增删改查、分配授权等操作。</div>
      </div>
      <a-button type="primary" @click="handleToCreateUserGroup"
        >创建用户组</a-button
      >
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入用户组名称或标识进行搜索"
      allow-clear
      v-model="searchKeyword"
      @search="handleGetUserGroupList(1, 15)"
      @keyup.enter.native="handleGetUserGroupList(1, 15)"
      @clear="handleGetUserGroupList(null)"
    />
    <div class="user-group-list">
      <a-table
        :bordered="false"
        :data="userGroupList"
        :pagination="userGroupListPagination"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
        :scroll="{ y: '100%' }"
      >
        <template #columns>
          <a-table-column
            title="用户组名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-groupname"
                @click="hantoToUserGroupDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="用户组标识"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              {{ record.code }}
            </template>
          </a-table-column>
          <a-table-column
            title="成员数"
            :width="120"
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              {{ record.memberNum }}
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
                    @click="handleDeleteUserGroup(record)"
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
