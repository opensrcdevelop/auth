import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import {
  getGroupResources,
  getResourceGroupDetail,
  updateResourceGroup,
} from "@/api/resourceGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { useRoute } from "vue-router";
import { Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("resource_group_info");

/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
const handleTabChange = (tabKey: string) => {
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
};

const resourceGroupId = ref("");
const resourceGroupName = ref("");

/** 资源组信息表单 */
const resourceGroupInfoFromRef = ref();
const resourceGroupInfoFrom = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
  desc: undefined,
});
const resourceGroupInfoFormRules = {
  name: [{ required: true, message: "资源组名称未填写" }],
  code: [
    { required: true, message: "资源组标识未填写" },
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
};

/**
 * 获取资源组详情
 *
 * @param id
 */
const handleGetResourceGroupDetail = (id: string) => {
  getResourceGroupDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        resourceGroupId.value = data.id;
        resourceGroupName.value = data.name;

        resourceGroupInfoFrom.id = data.id;
        resourceGroupInfoFrom.name = data.name;
        resourceGroupInfoFrom.code = data.code;
        resourceGroupInfoFrom.desc = data.desc;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源组详情");
    });
};

/** 组内资源列表 */
const resourceList = reactive([]);
const resourceSearchKeyword = ref("");
const resoueceListPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取组内资源列表
 *
 * @param id 资源组ID
 * @param page 页数
 * @param size 条数
 */
const handleGetGroupResourceList = (
  id: string,
  page: number = 1,
  size: number = 15
) => {
  getGroupResources(id, {
    page,
    size,
    keyword: resourceSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        resourceList.length = 0;
        resourceList.push(...data.list);

        resoueceListPagination.current = data.current;
        resoueceListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取组内资源列表");
    });
};

/**
 * 提交资源组信息表单
 */
const handleResourceGroupInfoFormSubmit = (formData: any) => {
  delete formData.code
  updateResourceGroup(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新资源组");
    });
};

/**
 * 重置资源组信息表单
 */
const handleResetResourceGroupInfoForm = () => {
  resourceGroupInfoFromRef.value.resetFields();
  handleGetResourceGroupDetail(resourceGroupId.value);
};

/**
 * 搜索组内资源
 */
const handleSearchResource = () => {
  handleGetGroupResourceList(resourceGroupId.value);
}

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  resoueceListPagination.current = page;
  handleGetGroupResourceList(resourceGroupId.value, page, resoueceListPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  resoueceListPagination.pageSize = size;
  handleGetGroupResourceList(resourceGroupId.value, 1, size);
};

/**
 * 跳转资源详情
 */
const handleToResourceDetail = (resource: any) => {
  router.push({
    path: "/permission/resource/detail/",
    query: {
      id: resource.id,
    }
  })
}

/**
 * 跳转创建资源
 */
const handleToCreateResource = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.resourceGroupId = resourceGroupId.value;
  globalVariables.saveData();
  router.push({
    path: "/permission/resource/create/"
  })
}

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      const resourceGroupId = route.query.id as string;
      handleGetResourceGroupDetail(resourceGroupId);
      handleGetGroupResourceList(resourceGroupId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      resourceGroupId,
      resourceGroupName,
      resourceGroupInfoFromRef,
      resourceGroupInfoFrom,
      resourceGroupInfoFormRules,
      handleResourceGroupInfoFormSubmit,
      handleResetResourceGroupInfoForm,
      resourceSearchKeyword,
      resourceList,
      resoueceListPagination,
      handleSearchResource,
      handlePageChange,
      handlePageSizeChange,
      handleToResourceDetail,
      handleToCreateResource
    };
  },
});
