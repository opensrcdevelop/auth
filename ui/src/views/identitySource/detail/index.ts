import {getIdentitySourceDetail, getUserBindingList, updateIdentitySource,} from "@/api/identitySource";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {generateRandomString, getOAuthIssuer, getQueryString, handleApiError, handleApiSuccess,} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {computed, defineComponent, onMounted, reactive, ref} from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("registration_info");

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

const handleTabInit = (tabKey: string, id: string = registrationId.value) => {
  switch (tabKey) {
    case "registration_info":
      handleGetRegistrationDetail(id);
      break;
    case "user_bindings":
      handleGetRegistrationDetail(id);
      handleGetUserBindingList(id);
  }
};

const registrationId = ref("");
const registrationName = ref("");

/**
 * 身份源信息表单
 */
const registrationInfoForm = reactive({
  name: undefined,
  code: undefined,
  clientId: undefined,
  clientSecret: undefined,
  clientAuthenticationMethod: undefined,
  authorizationGrantType: undefined,
  additionalParams: undefined,
});
const registrationInfoFormRef = ref(null);
const registrationInfoFormRules = {
  name: [{ required: true, message: "身份源显示名称未填写" }],
  code: [{ required: true, message: "身份源标识未填写" }],
  clientId: [{ required: true, message: "Client ID 未填写" }],
  clientSecret: [{ required: true, message: "Client Secret 未填写" }],
  clientAuthenticationMethod: [
    { required: true, message: "客户端认证方式未选择" },
  ],
  authorizationGrantType: [{ required: true, message: "授权类型未选择" }],
  additionalParams: [
    {
      validator: (value, cb) => {
        try {
          if (value) {
            JSON.parse(value);
            cb();
          }
        } catch (e) {
          cb("额外参数 JSON 格式错误");
        }
      },
    },
  ],
};

// 回调地址
const callBackUrl = computed(() => {
  return `${getOAuthIssuer()}/login/federation/callback/${
    registrationInfoForm.code ? registrationInfoForm.code : "身份源标识"
  }`;
});

/**
 * 生成随机身份源标识
 */
const generateRandomRegistraionCode = () => {
  registrationInfoForm.code = generateRandomString(10);
};

/**
 * 重置身份源信息表单
 */
const handleResetRegistrationInfoForm = () => {
  registrationInfoFormRef.value.resetFields();
  handleGetRegistrationDetail();
};

/**
 * 提交身份源信息表单
 */
const handleRegistrationInfoFormSubmit = (formData: any) => {
  const requestData = {
    ...formData,
    id: registrationId.value,
  }
  if (requestData.additionalParams) {
    requestData.additionalParams = JSON.parse(formData.additionalParams);
  } else {
    requestData.additionalParams = undefined;
  }

  updateIdentitySource(requestData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetRegistrationDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新身份源");
    });
};

/**
 * 获取身份源详情
 */
const handleGetRegistrationDetail = (id: string = registrationId.value) => {
  getIdentitySourceDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        registrationId.value = data.id;
        registrationName.value = data.name;

        registrationInfoForm.name = data.name;
        registrationInfoForm.code = data.code;
        registrationInfoForm.clientId = data.clientId;
        registrationInfoForm.clientSecret = data.clientSecret;
        registrationInfoForm.clientAuthenticationMethod =
          data.clientAuthenticationMethod;
        registrationInfoForm.authorizationGrantType =
          data.authorizationGrantType;
        registrationInfoForm.additionalParams = data.additionalParams;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取身份源详情");
    });
};

/**
 * 关联用户列表
 */
const userBindingList = reactive([]);
const userBindingSearchKeyword = ref(undefined);
let userBindingListPagination;

const handleGetUserBindingList = (
  id: string = registrationId.value,
  page: number = 1,
  size: number = 15
) => {
  getUserBindingList(id, {
    page,
    size,
    keyword: userBindingSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userBindingList.length = 0;
        userBindingList.push(...data.list);
        userBindingListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取身份源的关联用户");
    });
};

/**
 * 搜索用户绑定
 */
const handleSearchUserBinding = () => {
  handleGetUserBindingList();
};

/**
 * 跳转至用户详情
 */
const handleToUserDetail = (id: string) => {
  router.push({
    path: "/user/detail",
    query: {
      id,
    },
  });
};

export default defineComponent({
  setup() {
    const registrationId = getQueryString("id");
    userBindingListPagination = usePagination(
      `${registrationId}_userBindingList`,
      ({ page, size }) => {
        if (getQueryString("active_tab") === "user_bindings") {
          handleGetUserBindingList(registrationId, page, size);
        }
      }
    );

    onMounted(() => {
      const tabName = getQueryString("active_tab") || "registration_info";
      activeTab.value = tabName;
      handleTabInit(tabName, registrationId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      registrationId,
      registrationName,
      registrationInfoForm,
      registrationInfoFormRef,
      registrationInfoFormRules,
      callBackUrl,
      generateRandomRegistraionCode,
      handleResetRegistrationInfoForm,
      handleRegistrationInfoFormSubmit,
      userBindingList,
      userBindingSearchKeyword,
      userBindingListPagination,
      handleSearchUserBinding,
      handleToUserDetail,
    };
  },
});
