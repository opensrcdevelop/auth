<script lang="ts">
import roleTs from "./index";
export default roleTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="role-header">
      <div class="left">
        <div class="title">角色管理</div>
        <div class="info">
          进行角色的增删改查，角色的资源权限授权，用户 / 用户组的角色授权。
        </div>
      </div>
      <a-button type="primary" @click="handleToCreateRole">创建角色</a-button>
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入角色名称或标识进行搜索"
      allow-clear
      v-model="searchKeyword"
      @search="handleGetRoleList(1, 15)"
      @keyup.enter.native="handleGetRoleList(1, 15)"
      @clear="handleGetRoleList(1, 15)"
    />
    <div class="role-list">
      <a-table
        :data="roleList"
        :bordered="false"
        :pagination="roleListPagination"
        :scroll="{ y: '100%' }"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
      >
        <template #columns>
          <a-table-column
            title="角色名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-rolename"
                @click="handleToRoleDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="角色标识"
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
                    @click="handleDeleteRole(record)"
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
