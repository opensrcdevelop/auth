import { getTenantDetail, updateTenant } from "@/api/tenant";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
import { useRoute } from "vue-router";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("tanant_info");

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

const tenantId = ref("");
const tenantName = ref("");

/** 租户信息表单 */
const tenantInfoFormRef = ref();
const tenantInfoForm = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
  desc: undefined,
  enabled: undefined,
  createTime: undefined,
});
const tenantInfoFormFormRules = {
  name: [{ required: true, message: "租户名称未填写" }],
};

/** 端点信息 */
const endpointInfo = reactive({
  issuer: undefined,
  consoleUrl: undefined,
})

/**
 * 获取租户详情
 *
 * @param id 租户 ID
 */
const handleGetTenantDetail = (id: string) => {
  getTenantDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        tenantId.value = data.id;
        tenantName.value = data.name;

        tenantInfoForm.id = data.id;
        tenantInfoForm.name = data.name;
        tenantInfoForm.code = data.code;
        tenantInfoForm.desc = data.desc;
        tenantInfoForm.enabled = data.enabled;
        tenantInfoForm.createTime = data.createTime;

        endpointInfo.issuer = data.issuer;
        endpointInfo.consoleUrl = data.consoleUrl;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取租户详情");
    });
};

/**
 * 重置用户字段信息表单
 */
const handleResetTenantInfoForm = () => {
  tenantInfoFormRef.value.resetFields();
  handleGetTenantDetail(tenantId.value);
};

/**
 * 提交租户信息表单
 *
 * @param formData 租户信息表单
 */
const handleTenantInfoFormSubmit = (formData: any) => {
  delete formData.createTime;
  delete formData.code;

  updateTenant(formData)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        Notification.success("保存成功");
        handleGetTenantDetail(tenantId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新租户信息");
    });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      const tenantId = route.query.id as string;
      handleGetTenantDetail(tenantId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      tenantId,
      tenantName,
      tenantInfoForm,
      tenantInfoFormFormRules,
      tenantInfoFormRef,
      handleTenantInfoFormSubmit,
      handleResetTenantInfoForm,
      endpointInfo
    };
  },
});
