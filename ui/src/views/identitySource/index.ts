import {deleteIdentitySource, getIdentitySourceList, updateIdentitySource,} from "@/api/identitySource";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, reactive, ref} from "vue";

/** 身份源列表 */
const identitySourceList = reactive([]);
const searchKeyword = ref(null);
let identitySourceListPagination;

/**
 * 获取身份源列表
 */
const handleGetIdentitySourceList = (page: number = 1, size: number = 15) => {
  getIdentitySourceList({
    page,
    size,
    keyword: searchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        identitySourceList.length = 0;
        identitySourceList.push(...data.list);

        identitySourceListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取身份源列表");
    });
};

/**
 * 跳转到身份源详情
 */
const handleToIdentitySourceDetail = (identitySource: any) => {
  router.push({
    path: "/identitySource/detail",
    query: {
      id: identitySource.id,
      active_tab: "registration_info",
    },
  });
};

/**
 * 跳转到身份源提供商详情
 */
const handleToProviderDetail = (identitySource: any) => {
  router.push({
    path: "/identitySource/provider/detail",
    query: {
      id: identitySource.provider.id,
      active_tab: "provider_info",
    },
  });
};

/**
 * 更新身份源状态
 */
const handleUpdateIdentitySourceState = (identitySource: any) => {
  updateIdentitySource({
    id: identitySource.id,
    enabled: identitySource.enabled,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("更新身份源状态成功");
        handleGetIdentitySourceList();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新身份源状态");
    });
};

/**
 * 删除身份源
 */
const handleDeleteIdentitySource = (identitySource: any) => {
  Modal.warning({
    title: `确定删除身份源「${identitySource.name}」吗？`,
    content: "此操作将删除该身份源及与用户的绑定关系，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteIdentitySource(identitySource.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetIdentitySourceList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除身份源");
        });
    },
  });
};

export default defineComponent({
  setup() {
    identitySourceListPagination = usePagination(
      "identitySourceList",
      ({ page, size }) => {
        handleGetIdentitySourceList(page, size);
      }
    );

    return {
      identitySourceList,
      searchKeyword,
      identitySourceListPagination,
      handleGetIdentitySourceList,
      handleToIdentitySourceDetail,
      handleToProviderDetail,
      handleUpdateIdentitySourceState,
      handleDeleteIdentitySource
    };
  },
});
