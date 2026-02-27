import { getTenantDetail, updateTenant } from "@/api/tenant";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";

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
  effectiveTime: undefined,
  expirationTime: undefined,
  createTime: undefined,
});
const tenantInfoFormFormRules = {
  name: [{ required: true, message: "租户名称未填写" }],
  effectiveTime: [
    {
      validator: (value: any, cb: any) => {
        const expirationTime = tenantInfoForm.expirationTime;
        // 如果生效时间为空，或者失效时间为空，或者生效时间早于失效时间，则通过
        if (!value || !expirationTime || new Date(value) <= new Date(expirationTime)) {
          cb();
        } else {
          cb("生效时间必须早于失效时间");
        }
      },
      trigger: "change",
    },
  ],
  expirationTime: [
    {
      validator: (value: any, cb: any) => {
        const effectiveTime = tenantInfoForm.effectiveTime;
        // 如果生效时间为空，或者失效时间为空，或者生效时间早于失效时间，则通过
        if (!effectiveTime || !value || new Date(effectiveTime) <= new Date(value)) {
          cb();
        } else {
          cb("生效时间必须早于失效时间");
        }
      },
      trigger: "change",
    },
  ],
};

/** 端点信息 */
const endpointInfo = reactive({
  issuer: undefined,
  consoleUrl: undefined,
});

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
        tenantInfoForm.effectiveTime = data.effectiveTime;
        tenantInfoForm.expirationTime = data.expirationTime;
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
 * 生效时间或失效时间变化时触发验证
 */
const handleTimeChange = () => {
  tenantInfoFormRef.value.validateField(["effectiveTime", "expirationTime"]);
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
      activeTab.value = getQueryString("active_tab") || "tanant_info";
      handleGetTenantDetail(getQueryString("id"));
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
      handleTimeChange,
      endpointInfo,
    };
  },
});
