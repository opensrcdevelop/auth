import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { createResource } from "@/api/resource";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { getResourceGroupList } from "@/api/resourceGroup";
import { useGlobalVariablesStore } from "@/store/globalVariables";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 资源组列表 */
const resourceGroupList = reactive([]);
const resourceGroupListPagination = {
  current: 1,
  total: 0,
};
const resourceGroupSearchKeyword = ref("");
const handleGetResourceGroupList = (
  page: number = 1,
  size: number = 15
) => {
  getResourceGroupList({
    page,
    size,
    keyword: resourceGroupSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page === 1) {
          resourceGroupList.length = 0;
          resourceGroupList.push(...data.list);
        } else {
          resourceGroupList.push(...data.list);
        }
        resourceGroupListPagination.current = data.current;
        resourceGroupListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源组列表");
    });
};

/** 显示选择资源组 */
const showSelectResourceGroup = ref(true);

/**
 * 搜索资源组
 */
const handleSearchResourceGroup = () => {
  handleGetResourceGroupList(1);
};

let loadMoreResourceGroupLoading = false;
/**
 * 加载更多资源组
 */
const loadMoreResourceGroup = () => {
  if (loadMoreResourceGroupLoading) return;
  if (resourceGroupList.length < resourceGroupListPagination.total) {
    loadMoreResourceGroupLoading = true;
    resourceGroupListPagination.current++;
    handleGetResourceGroupList(resourceGroupListPagination.current);
    loadMoreResourceGroupLoading = false;
  }
}

/** 创建资源表单 */
const createResourceFormRef = ref();
const createResourceForm = reactive({
  name: undefined,
  code: undefined,
  desc: undefined,
  api: undefined,
  resourceGroupId: undefined,
});
const createResourceFormRules = {
  name: [{ required: true, message: "资源名称未填写" }],
  code: [
    { required: true, message: "资源标识未填写" },
    {
      validator: (value, cb) => {
        if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
          cb("只允许包含英文字母、数字、下划线_、横线-");
        } else {
          cb();
        }
      },
    },
  ],
  resourceGroupId: [{ required: true, message: "资源组未选择" }]
};

/**
 * 提交创建资源表单
 */
const handleCreateResourceFormSubmit = (formData: any) => {
  createResource(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateResourceForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建资源");
    });
};

/**
 * 重置创建资源表单
 */
const handleResetCreateResourceForm = () => {
  createResourceFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const globalVariables = useGlobalVariablesStore().getData();
      if (!globalVariables.resourceGroupId) {
        handleGetResourceGroupList();
        createResourceForm.resourceGroupId = undefined;
        showSelectResourceGroup.value = true;
      } else {
        createResourceForm.resourceGroupId = globalVariables.resourceGroupId;
        showSelectResourceGroup.value = false;
      }
    });

    return {
      handleBack,
      createResourceFormRef,
      createResourceForm,
      createResourceFormRules,
      handleCreateResourceFormSubmit,
      handleResetCreateResourceForm,
      resourceGroupList,
      handleSearchResourceGroup,
      resourceGroupSearchKeyword,
      loadMoreResourceGroup,
      showSelectResourceGroup
    };
  },
});
