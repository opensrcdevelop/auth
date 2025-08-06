import router from "@/router";
import {defineComponent, ref} from "vue";
import Chat from "./components/chat/Chat.vue";

const activeTab = ref("chat");

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

export default defineComponent({
  components: {
    Chat,
  },
  setup() {
    return {
      activeTab,
      handleTabChange,
    };
  },
});
