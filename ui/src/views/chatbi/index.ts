import router from "@/router";
import {defineComponent, onMounted, ref} from "vue";
import Chat from "./components/chat/Chat.vue";
import DataSourceManagement from "./components/datasource/DataSourceManagement.vue";
import {getQueryString} from "@/util/tool";

const activeTab = ref("chat");
const chatRef = ref();
const dataSourceManagementRef = ref();

/**
 * tab 切换事件
 */
const handleTabChange = (tabKey: string) => {
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
  handleTabInit(tabKey);
};

/**
 * tab 初始化
 */
const handleTabInit = (tabKey: string) => {
  switch (tabKey) {
    case "chat":
      chatRef.value?.init();
      break;
    case "data_source_management":
      dataSourceManagementRef.value?.init();
      break;
  }
};

export default defineComponent({
  components: {
    Chat,
    DataSourceManagement,
  },
  setup() {
    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "chat";
      handleTabInit(activeTab.value);
    });

    return {
      activeTab,
      handleTabChange,
      chatRef,
      dataSourceManagementRef,
    };
  },
});
