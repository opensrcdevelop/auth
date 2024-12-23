import { defineStore } from "pinia";

/** 定义全局变量 */
export const useGlobalVariablesStore = defineStore("globalVariables", {
  state() {
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
      /** 权限表达式 */
      permissionExp: undefined,
    };
  },
  actions: {
    saveData() {
      localStorage.setItem("globalVariables", JSON.stringify(this.$state));
    },
    getData() {
      const globalVariables = localStorage.getItem("globalVariables");
      if (globalVariables) {
        return JSON.parse(globalVariables);
      } else {
        return this.$state;
      }
    },
  },
});
