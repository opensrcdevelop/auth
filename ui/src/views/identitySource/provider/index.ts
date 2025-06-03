import {getIdentitySourceProviderList} from "@/api/identitySource";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {defineComponent, onMounted, reactive, ref} from "vue";

// 提供商名称或标识检索关键字
const searchKeyword = ref(undefined);

// 提供商列表
const providerList = reactive([]);
const handleGetProviderList = () => {
  getIdentitySourceProviderList({
    keyword: searchKeyword.value,
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        providerList.length = 0;
        providerList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取身份源提供商列表");
    });
};

/**
 * 跳转提供商详情
 *
 * @param provider 提供商
 */
const handleToProviderDetail = (provider: any) => {
  router.push({
    path: "/identitySource/provider/detail",
    query: {
      id: provider.id,
      active_tab: "provider_info",
    },
  });
};

/**
 * 跳转创建提供商
 */
const handleToCreateProvider = () => {
  router.push({
    path: "/identitySource/provider/create",
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetProviderList();
    });

    return {
      searchKeyword,
      providerList,
      handleGetProviderList,
      handleToProviderDetail,
      handleToCreateProvider,
    };
  },
});
