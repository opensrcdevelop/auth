import { getIdentitySourceProviderList } from "@/api/identitySource";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
// 提供商名称或标识检索关键字
var searchKeyword = ref(undefined);
// 提供商列表
var providerList = reactive([]);
var handleGetProviderList = function () {
    getIdentitySourceProviderList({
        keyword: searchKeyword.value,
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            providerList.length = 0;
            providerList.push.apply(providerList, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取身份源提供商列表");
    });
};
/**
 * 跳转提供商详情
 *
 * @param provider 提供商
 */
var handleToProviderDetail = function (provider) {
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
var handleToCreateProvider = function () {
    router.push({
        path: "/identitySource/provider/create",
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            handleGetProviderList();
        });
        return {
            searchKeyword: searchKeyword,
            providerList: providerList,
            handleGetProviderList: handleGetProviderList,
            handleToProviderDetail: handleToProviderDetail,
            handleToCreateProvider: handleToCreateProvider,
        };
    },
});
