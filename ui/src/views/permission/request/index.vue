<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div class="permission-request-container">
    <div class="page-header">
      <div class="title">权限申请审批</div>
      <div class="info">
        查看和管理用户的权限申请。
      </div>
    </div>

    <div class="table-toolbar">
      <a-input-search
        :style="{ width: '320px' }"
        placeholder="输入申请人用户名进行搜索"
        allow-clear
        v-model="searchKeyword"
        @search="handleGetRequestList(1, 15)"
        @keyup.enter="handleGetRequestList(1, 15)"
        @clear="handleGetRequestList(1, 15)"
      />
      <a-checkbox v-model="pendingOnly" @change="handlePendingOnlyChange">
        只查看待审批的权限申请
      </a-checkbox>
    </div>

    <div class="table-card">
      <a-table
        :data="requestList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="requestListPagination.pagination"
        @page-change="requestListPagination.handlePageChange"
        @page-size-change="requestListPagination.handlePageSizeChange"
      >
        <template #columns>
          <a-table-column title="申请人" ellipsis tooltip>
            <template #cell="{ record }">
              <span
                class="table-column-username"
                @click="handleToUserDetail(record)"
              >
                {{ record.username || record.userId }}
              </span>
            </template>
          </a-table-column>
          <a-table-column title="申请时间" ellipsis tooltip>
            <template #cell="{ record }">{{ record.requestTime }}</template>
          </a-table-column>
          <a-table-column title="申请理由" ellipsis tooltip>
            <template #cell="{ record }">{{ record.reason || "-" }}</template>
          </a-table-column>
          <a-table-column title="待审批">
            <template #cell="{ record }">
              <a-tag color="orange">{{ record.pendingCount }}</a-tag>
            </template>
          </a-table-column>
          <a-table-column title="总计">
            <template #cell="{ record }">
              <a-tag color="arcoblue">{{ record.totalCount }}</a-tag>
            </template>
          </a-table-column>
          <a-table-column title="操作">
            <template #cell="{ record }">
              <a-button type="text" size="small" @click="handleToRequestDetail(record)">
                <template #icon>
                  <icon-eye />
                </template>
                <template #default>查看</template>
              </a-button>
            </template>
          </a-table-column>
        </template>
      </a-table>
    </div>
  </div>
</template>
