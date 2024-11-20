<script lang="ts">
import tenantTs from "./index";
export default tenantTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="tenant-header">
      <div class="left">
        <div class="title">多租户</div>
        <div class="info">
          提供租户维度的用户身份认证授权及管理等功能，不同租户间数据完全隔离。
        </div>
      </div>
      <a-button type="primary" @click="handleToCreateTenant">创建租户</a-button>
    </div>
    <a-tabs default-active-key="1">
      <a-tab-pane key="1" title="租户列表">
        <div class="tenant-search">
          <a-input-search
            v-model="tenantSerachKeyword"
            :style="{ width: '320px' }"
            placeholder="输入租户名称或标识进行搜索"
            allow-clear
            @search="handleGetTenantList(1, 15)"
            @keyup.enter.native="handleGetTenantList(1, 15)"
            @clear="handleGetTenantList(1, 15)"
          />
        </div>
        <div class="tenant-list">
          <a-table
            :data="tenantList"
            :bordered="false"
            :scroll="{ y: '100%' }"
            :pagination="tenantPagination"
            @page-change="handlePageChange"
            @page-size-change="handlePageSizeChange"
          >
            <template #columns>
              <a-table-column
                title="租户名称"
                ellipsis
                tooltip
                :sortable="{
                  sortDirections: ['ascend', 'descend'],
                }"
              >
                <template #cell="{ record }">
                  <span
                    class="table-column-tenantname"
                    @click="handleToTenantDetail(record)"
                  >
                    {{ record.name }}
                  </span>
                </template>
              </a-table-column>
              <a-table-column
                title="租户标识"
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
                        @click="handleDeleteTenant(record)"
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
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
