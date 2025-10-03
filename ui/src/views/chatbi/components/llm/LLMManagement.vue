<template>
  <div class="header">
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入模型提供商名称进行搜索"
      allow-clear
      v-model="modelProviderSearchKeyword"
      @search="handleGetModelProviderList(1, 15)"
      @keyup.enter.native="handleGetModelProviderList(1, 15)"
      @clear="handleGetModelProviderList(1, 15)"
    />
    <a-button type="primary">创建模型提供商</a-button>
  </div>
  <div class="model-provider-list">
    <a-table
      :data="modelProviderList"
      :bordered="false"
      :scroll="{ y: '100%' }"
      :pagination="modelProviderListPagination.pagination"
      @page-change="modelProviderListPagination.handlePageChange"
      @page-size-change="modelProviderListPagination.handlePageSizeChange"
    >
      <template #columns>
        <a-table-column
          title="模型提供商名称"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span
              class="table-column-name"
              @click="handleToModelProviderDetail(record)"
            >
              {{ record.name }}
            </span>
          </template>
        </a-table-column>
        <a-table-column
          title="模型提供商类型"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span>
              {{ record.type }}
            </span>
          </template>
        </a-table-column>
        <a-table-column title="是否启用">
          <template #cell="{ record }">
            <a-switch
              type="round"
              size="small"
              v-model="record.enabled"
              @change="handleUpdateModelProviderState(record)"
            />
          </template>
        </a-table-column>
        <a-table-column title="操作" fixed="right" :width="60">
          <template #cell="{ record }">
            <a-dropdown>
              <a-button type="text">
                <template #icon>
                  <icon-more />
                </template>
              </a-button>
              <template #content>
                <a-doption style="color: #e8353e">
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
</template>

<script setup lang="ts">
import {getModelProviderList, updateModelProvider} from "@/api/chatbi";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {reactive, ref} from "vue";

/** 模型提供商列表 */
const modelProviderList = reactive([]);
const modelProviderSearchKeyword = ref(null);
let modelProviderListPagination = usePagination(
  "modelProviderList",
  ({ page, size }) => {
    handleGetModelProviderList(page, size);
  }
);

/**
 * 获取模型提供商列表
 */
const handleGetModelProviderList = (page: number = 1, size: number = 15) => {
  getModelProviderList({
    page,
    size,
    keyword: modelProviderSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        modelProviderList.length = 0;
        modelProviderList.push(...data.list);

        modelProviderListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取模型提供商列表");
    });
};

/**
 * 更新数据源启用状态
 */
const handleUpdateModelProviderState = (modelProvider: any) => {
  updateModelProvider({
    id: modelProvider.id,
    enabled: modelProvider.enabled,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("更新模型提供商启用状态成功");
        handleGetModelProviderList();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新模型提供商启用状态");
    });
};

/**
 * 跳转到数据源详情
 */
const handleToModelProviderDetail = (modelProvider: any) => {
  router.push({
    path: "/chatbi/llm/detail",
    query: {
      id: modelProvider.id,
      active_tab: "model_provider_info",
    },
  });
};

/**
 * 初始化
 */
const init = () => {
  handleGetModelProviderList(
    modelProviderListPagination.pagination.current,
    modelProviderListPagination.pagination.pageSize
  );
};

defineExpose({
  init,
});
</script>

<style lang="scss" scoped>
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.model-provider-list {
  margin-top: 16px;
}

.table-column-name {
  cursor: pointer;
  color: #215ae5;
}
</style>
