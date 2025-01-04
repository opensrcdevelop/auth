import { deleteResourceGroup, getResourceGroupList } from "@/api/resourceGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";

/** 资源列表 */
const resourceGroupList = reactive([]);
const resourceGroupSearchKeyword = ref(null);
let resourceGroupListPagination;

/**
 * 获取资源组列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetResourceGroupList = (page: number = 1, size: number = 15) => {
  getResourceGroupList({
    page,
    size,
    keyword: resourceGroupSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        resourceGroupList.length = 0;
        resourceGroupList.push(...data.list);

        resourceGroupListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源列表");
    });
};

/**
 * 跳转到资源组详情
 */
const handleToResourceGroupDetail = (resourceGroup: any) => {
  router.push({
    path: "/resource/group/detail",
    query: {
      id: resourceGroup.id,
      active_tab: "resource_group_info",
    },
  });
};

/**
 * 删除资源组
 */
const handleDeleteResourceGroup = (resourceGroup: any) => {
  Modal.warning({
    title: `确定删除资源组「${resourceGroup.name}」吗？`,
    content:
      "此操作将删除该资源组内的所有资源，资源关联的所有权限及权限授权，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteResourceGroup(resourceGroup.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetResourceGroupList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除资源组");
        });
    },
  });
};

/**
 * 跳转资源组
 */
const handleToCreateResourceGroup = () => {
  router.push({
    path: "/resource/group/create",
  });
};

export default defineComponent({
  setup() {
    resourceGroupListPagination = usePagination(
      "resourceGroupList",
      ({ page, size }) => {
        handleGetResourceGroupList(page, size);
      }
    );

    onMounted(() => {
      handleGetResourceGroupList(
        resourceGroupListPagination.pagination.current,
        resourceGroupListPagination.pagination.pageSize
      );
    });

    return {
      resourceGroupList,
      resourceGroupSearchKeyword,
      resourceGroupListPagination,
      handleGetResourceGroupList,
      handleToResourceGroupDetail,
      handleDeleteResourceGroup,
      handleToCreateResourceGroup,
    };
  },
});
