<template>
  <div class="my-permissions">
    <div class="search-wrapper">
      <a-input-search
        v-model="searchKeyword"
        placeholder="输入权限名称、资源名称或资源组名称进行搜索"
        allow-clear
        @search="handleSearch"
        @change="handleSearchChange"
      />
    </div>
    <a-table
      :data="filteredPermissions"
      :bordered="false"
      :pagination="{
        showTotal: true,
        showPageSize: true,
        defaultPageSize: 10,
        pageSizeOptions: [10, 50, 100],
      }"
    >
      <template #columns>
        <a-table-column
          title="权限"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span class="permission-text">
              {{ record.resourceGroupName }} / {{ record.resourceName }} /
              {{ record.permissionName }}</span
            >
          </template>
        </a-table-column>
        <a-table-column
          title="授权时间"
          data-index="authorizeTime"
          :width="180"
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        />
      </template>
    </a-table>
  </div>
</template>

<script setup lang="ts">
import {computed, ref, watch} from "vue";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {getMyPermissions} from "@/api/permission";

const permissions = ref([]);
const searchKeyword = ref("");

const props = withDefaults(
  defineProps<{
    activeKey?: string;
  }>(),
  {
    activeKey: "",
  },
);

// 加载用户权限列表
const handleLoadPermissions = () => {
  getMyPermissions()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissions.value = data || [];
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取我的权限");
    });
};

// 执行初始化
const handleInit = () => {
  handleLoadPermissions();
};

// 监听 tab 切换
watch(
  () => props.activeKey,
  (newActiveKey) => {
    if (newActiveKey === "my_permissions") {
      handleInit();
    }
  },
  { immediate: true },
);

// 检索逻辑
const filteredPermissions = computed(() => {
  if (!searchKeyword.value) {
    return permissions.value;
  }
  const keyword = searchKeyword.value.toLowerCase();
  return permissions.value.filter(
    (item: any) =>
      item.permissionName?.toLowerCase().includes(keyword) ||
      item.resourceName?.toLowerCase().includes(keyword) ||
      item.resourceGroupName?.toLowerCase().includes(keyword),
  );
});

const handleSearch = () => {
  // 检索由 computed 属性自动处理
};

const handleSearchChange = () => {
  // 检索由 computed 属性自动处理
};
</script>

<style scoped>
.my-permissions {
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.search-wrapper {
  max-width: 380px;
}

.permission-text {
  font-size: 12px;
  font-weight: 500;
  padding: 4px 8px;
  border-radius: 2px;
  background-color: var(--color-neutral-3);
}
</style>
