import { defineStore } from "pinia";
/** 定义全局变量 */
export var useGlobalVariablesStore = defineStore("globalVariables", {
    state: function () {
        return {
            /** API 加载状态 */
            apiLoading: false,
            /** 资源ID */
            resourceId: "",
            /** 授权选项 */
            authorizeOptions: {
                principalId: undefined,
                principal: undefined,
                principalType: undefined,
                resourceGroup: undefined,
            },
            /** 资源组ID */
            resourceGroupId: undefined,
            /** 字典ID */
            dictId: undefined,
            /** 身份提供商 */
            identitySourceProvider: {
                id: undefined,
                name: undefined
            },
        };
    },
    actions: {
        saveData: function () {
            localStorage.setItem("globalVariables", JSON.stringify(this.$state));
        },
        getData: function () {
            var globalVariables = localStorage.getItem("globalVariables");
            if (globalVariables) {
                return JSON.parse(globalVariables);
            }
            else {
                return this.$state;
            }
        },
    },
});
