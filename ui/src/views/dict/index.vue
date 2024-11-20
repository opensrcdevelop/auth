<script lang="ts">
import indexTs from "./index";
export default indexTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="dict-header">
      <div class="left">
        <div class="title">数据字典</div>
        <div class="info">集中数据元素，提供统一管理。</div>
      </div>
      <a-button type="primary" @click="handleToCreateDict">创建字典</a-button>
    </div>
    <a-tabs default-active-key="1">
      <a-tab-pane key="1" title="字典列表">
        <div class="dict-search">
          <a-input-search
            v-model="dictSerachKeyword"
            :style="{ width: '320px' }"
            placeholder="输入字典名称或标识进行搜索"
            allow-clear
            @search="handleGetDictList(1, 15)"
            @keyup.enter.native="handleGetDictList(1, 15)"
            @clear="handleGetDictList(1, 15)"
          />
        </div>
        <div class="dict-list">
          <a-table
            :data="dictList"
            :bordered="false"
            :scroll="{ y: '100%' }"
            :pagination="dictPagination"
            @page-change="handlePageChange"
            @page-size-change="handlePageSizeChange"
          >
            <template #columns>
              <a-table-column
                title="字典名称"
                ellipsis
                tooltip
                :sortable="{
                  sortDirections: ['ascend', 'descend'],
                }"
              >
                <template #cell="{ record }">
                  <span
                    class="table-column-dictname"
                    @click="handleToDictDetail(record)"
                  >
                    {{ record.name }}
                  </span>
                </template>
              </a-table-column>
              <a-table-column
                title="字典标识"
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
                title="字典数据条数"
                ellipsis
                tooltip
                :sortable="{
                  sortDirections: ['ascend', 'descend'],
                }"
              >
                <template #cell="{ record }">
                  <span>
                    {{ record.dataCnt }}
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
                        @click="handleDeleteDict(record)"
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
