import { deleteDict, getDictList } from "@/api/dict";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";

/** 字典列表 */
const dictList = reactive([]);
const dictSerachKeyword = ref(null);
let dictPagination;

/**
 * 获取字典列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetDictList = (page: number = 1, size: number = 15) => {
  getDictList({
    page,
    size,
    keyword: dictSerachKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictList.length = 0;
        dictList.push(...data.list);

        dictPagination.updatePagination(data.current, data.total, data.size);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典列表");
    });
};

/**
 * 跳转字典详情
 *
 * @param dict 字典
 */
const handleToDictDetail = (dict: any) => {
  router.push({
    path: "/dict/detail",
    query: {
      id: dict.id,
      active_tab: "dict_info",
    },
  });
};

/**
 * 跳转创建字典
 *
 */
const handleToCreateDict = () => {
  router.push({
    path: "/dict/create",
  });
};

/**
 * 删除字典
 */
const handleDeleteDict = (dict: any) => {
  Modal.warning({
    title: `确定删除字典「${dict.name}」吗？`,
    content: "此操作将删除该字典数据关联的字典数据及用户字典，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteDict(dict.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetDictList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除字典");
        });
    },
  });
};

export default defineComponent({
  setup() {
    dictPagination = usePagination("dictList", ({ page, size }) => {
      handleGetDictList(page, size);
    });

    return {
      dictList,
      dictSerachKeyword,
      dictPagination,
      handleGetDictList,
      handleToDictDetail,
      handleToCreateDict,
      handleDeleteDict,
    };
  },
});
