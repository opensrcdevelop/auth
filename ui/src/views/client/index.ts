import { deleteClient, getClientList } from "@/api/client";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";

const clientList = reactive([]);
const searchKeyword = ref("");

/**
 * 获取客户端列表
 */
const handleGetClientList = (
  keyword: string,
  page: number = 1,
  size: number = -1
) => {
  clientList.length = 0;

  const query = {
    keyword,
    page,
    size,
  };

  // 去除无效的关键字
  if (keyword.trim().length === 0) {
    delete query.keyword;
  }

  getClientList(query)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        clientList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取客户端列表");
    });
};

/**
 * 跳转至客户端详情页
 *
 * @param id
 */
const toClientDetail = (id: string) => {
  router.push({
    path: "/client/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转至创建客户端页
 */
const toCreateClient = () => {
  router.push({
    path: "/client/create",
  });
};

/**
 * 删除客户端
 *
 * @param client 客户端
 */
const handleDeleteClient = (client: any) => {
  Modal.warning({
    title: `确定删除客户端「${client.name}」吗？`,
    content: "删除后将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteClient(client.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetClientList("");
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除客户端");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetClientList("");
    });

    return {
      clientList,
      searchKeyword,
      handleGetClientList,
      toClientDetail,
      toCreateClient,
      handleDeleteClient,
    };
  },
});
