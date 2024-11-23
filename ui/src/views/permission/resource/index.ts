import { deleteResource, getResourceList } from "@/api/resource";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";

/** 资源列表 */
const resourceList = reactive([]);
const resourceSearchKeyword = ref(null);
const resourceListPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取资源列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetResourceList = (page: number = 1, size: number = 15) => {
  getResourceList({
    page,
    size,
    keyword: resourceSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        resourceList.length = 0;
        resourceList.push(...data.list);

        resourceListPagination.current = data.current;
        resourceListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源列表");
    });
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  resourceListPagination.current = page;
  handleGetResourceList(page, resourceListPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  resourceListPagination.pageSize = size;
  handleGetResourceList(1, size);
};

/**
 * 跳转资源详情
 *
 * @param resource 资源
 */
const handleToResourceDetail = (resource: any) => {
  router.push({
    path: "/permission/resource/detail",
    query: {
      id: resource.id,
    },
  });
};

/**
 * 跳转创建资源
 */
const handleToCreateResource = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.resourceGroupId = undefined;
  globalVariables.saveData();
  router.push({
    path: "/permission/resource/create",
  });
};

/**
 * 删除资源
 */
const handleDeleteResource = (resource: any) => {
  Modal.warning({
    title: `确定删除资源「${resource.name}」吗？`,
    content: "此操作将删除该资源关联的权限及权限关联的授权，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteResource(resource.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetResourceList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除资源");
        });
    },
  });
};


/** 授权对话框 */
const authorizeVisible = ref(false);

/**
 * 授权
 */
const handleAuthorize = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.authorizeOptions.principal = undefined
  globalVariables.authorizeOptions.principalId = undefined
  globalVariables.authorizeOptions.principalType = undefined

  authorizeVisible.value = true;
}

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetResourceList();
    });

    return {
      resourceList,
      resourceSearchKeyword,
      resourceListPagination,
      handleGetResourceList,
      handlePageChange,
      handlePageSizeChange,
      handleToResourceDetail,
      handleToCreateResource,
      handleDeleteResource,
      authorizeVisible,
      handleAuthorize,
    };
  },
});
