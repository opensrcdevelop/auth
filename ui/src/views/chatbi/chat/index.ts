import {computed, defineComponent, onMounted, reactive, ref} from "vue";
import Chat from "../components/chat/Chat.vue";
import ChatHistory from "../components/chat/ChatHistory.vue";
import {getCurrentUser} from "@/api/user";
import {logoutSubmit} from "@/api/logout";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import router from "@/router";
import {AUTH_TOKENS} from "@/util/constants";
import {useGlobalVariablesStore} from "@/store/globalVariables";

const globalVariables = useGlobalVariablesStore();
const chatRef = ref();
const chatHistoryRef = ref();
const chatId = ref("");
const dataSourceId = ref("");

/** 用户信息相关 */
const currentUser = reactive({
  id: "",
  username: "",
});

/** 当前会话标题 */
const currentChatTitle = computed(() => {
  if (!chatId.value || !chatHistoryRef.value?.chatHistoryList) {
    return "";
  }
  const chat = chatHistoryRef.value.chatHistoryList.find(
    (item: any) => item.id === chatId.value,
  );
  return chat?.title || "";
});

/**
 * 获取当前用户信息
 */
const handleGetCurrentUser = () => {
  getCurrentUser()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        currentUser.id = data.id;
        currentUser.username = data.username;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取当前用户信息");
    });
};

/**
 * 退出登录
 */
const handleLogout = () => {
  Modal.warning({
    title: "确定退出登录？",
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "warning",
    },
    onOk: () => {
      logoutSubmit()
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("退出成功");
            localStorage.removeItem(AUTH_TOKENS);
            router.push({
              path: "/oauth2/redirect",
            });
          });
        })
        .catch((err: any) => {
          handleApiError(err, "退出登录");
        });
    },
  });
};

const handleSwitchChat = (id: string) => {
  chatId.value = id;
};

const handleAddNewChat = () => {
  chatId.value = "";
  dataSourceId.value = "";
};

const handleUpdateChatHistory = (id: string) => {
  chatId.value = id;
  chatHistoryRef.value?.init(id);
};

const handleUpdateDataSourceId = (id: string) => {
  dataSourceId.value = id;
};

const handleToUserHome = () => {
  router.push({
    path: "/user/home",
  });
};

export default defineComponent({
  components: {
    Chat,
    ChatHistory,
  },
  setup() {
    onMounted(() => {
      chatRef.value?.init();
      chatHistoryRef.value?.init();
      handleGetCurrentUser();
    });

    return {
      globalVariables,
      chatRef,
      chatHistoryRef,
      chatId,
      dataSourceId,
      handleSwitchChat,
      handleAddNewChat,
      handleUpdateChatHistory,
      handleUpdateDataSourceId,
      handleLogout,
      currentUser,
      currentChatTitle,
      handleToUserHome,
    };
  },
});
