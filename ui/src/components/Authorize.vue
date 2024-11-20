<template>
  <a-modal
    :visible="visible"
    :footer="false"
    @cancel="handleCloseModal"
    :width="680"
  >
    <template #title>创建授权</template>
    <a-form
      :model="selectResourceGroupForm"
      :rules="selectResourceGroupFormRules"
      ref="selectResourceGroupFormRef"
      @submit-success="handleToAuthorize"
    >
      <a-form-item field="resourceGroupId" label="资源组">
        <a-select
          placeholder="请选择资源组"
          allow-clear
          allow-search
          v-model:model-value="selectResourceGroupForm.resourceGroupId"
          v-model:input-value="resourceGroupSearchKeyword"
          @search="handleSearchResourceGroup"
          @clear="handleSearchResourceGroup"
          :filter-option="false"
          @dropdown-reach-bottom="loadMoreResourceGroup"
        >
          <a-option
            v-for="resourceGroup in resourceGroupList"
            :key="resourceGroup.id"
            :value="resourceGroup.id"
          >
            {{ resourceGroup.name }} - {{ resourceGroup.code }}
          </a-option>
        </a-select>
      </a-form-item>
      <a-form-item hide-label>
        <div class="btn-container">
          <a-space>
            <a-button @click="handleCloseModal">取消</a-button>
            <a-button type="primary" html-type="submit">确定</a-button>
          </a-space>
        </div>
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<script setup lang="ts">
import { reactive, ref } from "vue";
import { getResourceGroupList } from "@/api/resourceGroup";
import { handleApiSuccess } from "@/util/tool";
import { onMounted } from "vue";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";

const props = withDefaults(
  defineProps<{
    visible: boolean;
  }>(),
  {
    visible: false,
  }
);

const emits = defineEmits<{
  (e: "close"): void;
}>();

/** 选择资源组表单 */
const selectResourceGroupFormRef = ref();
const selectResourceGroupForm = reactive({
  resourceGroupId: undefined,
});
const selectResourceGroupFormRules = {
  resourceGroupId: [{ required: true, message: "请选择资源组" }],
};

/** 资源组列表 */
const resourceGroupList = reactive([]);
const resourceGroupListPagination = {
  current: 1,
  total: 0,
};
const resourceGroupSearchKeyword = ref("");

/**
 * 获取资源组列表
 */
const handleGetResourceGroupList = (page: number = 1, size: number = 15) => {
  getResourceGroupList({
    page,
    size,
    keyword: resourceGroupSearchKeyword.value,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      if (resourceGroupListPagination.current === 1) {
        resourceGroupList.length = 0;
        resourceGroupList.push(...data.list);
      } else {
        resourceGroupList.push(...data.list);
      }
      resourceGroupListPagination.total = data.total;
    });
  });
};

let loadMoreResourceGroupLoading = false;
/** 加载更多资源组 */
const loadMoreResourceGroup = () => {
  if (loadMoreResourceGroupLoading) return;
  if (resourceGroupList.length < resourceGroupListPagination.total) {
    loadMoreResourceGroupLoading = true;
    resourceGroupListPagination.current++;
    handleGetResourceGroupList(resourceGroupListPagination.current);
    loadMoreResourceGroupLoading = false;
  }
};

/** 搜索资源组 */
const handleSearchResourceGroup = () => {
  handleGetResourceGroupList(1);
};

/** 关闭对话框 */
const handleCloseModal = () => {
  selectResourceGroupFormRef.value.resetFields();
  emits("close");
};

/**
 * 跳转到授权页面
 */
const handleToAuthorize = () => {
  const globalVariblesStore = useGlobalVariablesStore();
  const resourceGroup = resourceGroupList.find(
    (item: any) => item.id === selectResourceGroupForm.resourceGroupId
  );
  globalVariblesStore.authorizeOptions.resourceGroup = resourceGroup;
  globalVariblesStore.saveData()
  handleCloseModal();

  router.push({
    path: "/permission/authorize",
  });
};

onMounted(() => {
  handleGetResourceGroupList();
});
</script>

<style scoped lang="scss">
.btn-container {
  width: 100%;
  display: flex;
  justify-content: flex-end;
}
</style>
