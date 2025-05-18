import {checkPassword, getPasswordPolicyDdetail, updatePasswordPolicy,} from "@/api/setting";
import {searchUser} from "@/api/user";
import {getUserGroupList} from "@/api/userGroup";
import router from "@/router";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {computed, defineComponent, onMounted, reactive, ref} from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  handleRestUpdatePasswordPolicyForm();
  handleResetPasswordStrengthForm();
  handleResetForceChangePasswordForm();
  handleResetCheckRes();
  router.back();
};

const passwordPolicyName = ref("");
const passwordPoliyId = ref("");

const updatePasswordPolicyForm = reactive({
  id: undefined,
  name: undefined,
  desc: undefined,
  passwordStrength: 0,
  minLength: 6,
  maxLength: 35,
  requireNumber: false,
  requireLowerCase: false,
  requireUpperCase: false,
  requireSpecialChar: false,
  minCharTypeCount: 0,
  prohibitUserInfo: false,
  prohibitSingleChar: false,
  prohibitConsecutiveChar: false,
  prohibitContainConsecutiveChar: false,
  minConsecutiveCharLength: 2,
  prohibitContainRepeatChar: false,
  minRepeatCharLength: 2,
  prohibitSpecificPassword: false,
  prohibitedPasswordList: [],
  enablePasswordDetection: false,
  enableForceChangePassword: false,
  forcedCycle: 1,
  forcedCycleUnit: "MONTH",
  remindCycle: 7,
  remindCycleUnit: "DAY",
  userIds: [],
  userGroupIds: [],
});

const handleRestUpdatePasswordPolicyForm = () => {
  updatePasswordPolicyForm.id = undefined;
  updatePasswordPolicyForm.name = undefined;
  updatePasswordPolicyForm.desc = undefined;
  updatePasswordPolicyForm.passwordStrength = 0;
  updatePasswordPolicyForm.minLength = 6;
  updatePasswordPolicyForm.maxLength = 35;
  updatePasswordPolicyForm.requireNumber = false;
  updatePasswordPolicyForm.requireLowerCase = false;
  updatePasswordPolicyForm.requireUpperCase = false;
  updatePasswordPolicyForm.requireSpecialChar = false;
  updatePasswordPolicyForm.minCharTypeCount = 0;
  updatePasswordPolicyForm.prohibitUserInfo = false;
  updatePasswordPolicyForm.prohibitSingleChar = false;
  updatePasswordPolicyForm.prohibitConsecutiveChar = false;
  updatePasswordPolicyForm.prohibitContainConsecutiveChar = false;
  updatePasswordPolicyForm.minConsecutiveCharLength = 2;
  updatePasswordPolicyForm.prohibitContainRepeatChar = false;
  updatePasswordPolicyForm.minRepeatCharLength = 2;
  updatePasswordPolicyForm.prohibitSpecificPassword = false;
  updatePasswordPolicyForm.prohibitedPasswordList = [];
  updatePasswordPolicyForm.enablePasswordDetection = false;
  updatePasswordPolicyForm.enableForceChangePassword = false;
  updatePasswordPolicyForm.forcedCycle = 1;
  updatePasswordPolicyForm.forcedCycleUnit = "MONTH";
  updatePasswordPolicyForm.remindCycle = 7;
  updatePasswordPolicyForm.remindCycleUnit = "DAY";
  updatePasswordPolicyForm.userIds = [];
  updatePasswordPolicyForm.userGroupIds = [];
}

/**
 * 基础信息
 */
const basicInfoForm = reactive({
  name: "",
  desc: "",
});
const basicInfoFormRef = ref(null);
const basicInfoFormRules = {
  name: [{ required: true, message: "策略名称未输入" }],
};

/**
 * 密码强度
 */
const passwordStrengthForm = reactive({
  passwordStrength: 0,
  minLength: 6,
  maxLength: 35,
  charType: [],
  minCharTypeCount: 0,
  prohibitUserInfo: false,
  prohibitSingleChar: false,
  prohibitConsecutiveChar: false,
  prohibitContainConsecutiveChar: false,
  minConsecutiveCharLength: 2,
  prohibitContainRepeatChar: false,
  minRepeatCharLength: 2,
  prohibitSpecificPassword: false,
  specificPasswordList: [],
  enablePasswordDetection: false,
});
const passwordStrengthFormRef = ref(null);
const passwordStrengthFormRules = {
  minLength: [{ required: true, message: "最小长度未输入" }],
  maxLength: [{ required: true, message: "最大长度未输入" }],
  charType: [{ required: true, message: "密码字符类型要求未选择" }],
};

const handleResetPasswordStrengthForm = () => {
  passwordStrengthForm.passwordStrength = 0;
  passwordStrengthForm.minLength = 6;
  passwordStrengthForm.maxLength = 35;
  passwordStrengthForm.charType = [];
  passwordStrengthForm.minCharTypeCount = 0;
  passwordStrengthForm.prohibitUserInfo = false;
  passwordStrengthForm.prohibitSingleChar = false;
  passwordStrengthForm.prohibitConsecutiveChar = false;
  passwordStrengthForm.prohibitContainConsecutiveChar = false;
  passwordStrengthForm.minConsecutiveCharLength = 2;
  passwordStrengthForm.prohibitContainRepeatChar = false;
  passwordStrengthForm.minRepeatCharLength = 2;
  passwordStrengthForm.prohibitSpecificPassword = false;
  passwordStrengthForm.specificPasswordList = [];
  passwordStrengthForm.enablePasswordDetection = false;
};

const passwordStrengthLabel = computed(() => {
  let label = "";
  switch (passwordStrengthForm.passwordStrength) {
    case 0:
      label = "无要求";
      break;
    case 1:
      label = "低强度";
      break;
    case 2:
      label = "中强度";
      break;
    case 3:
      label = "高强度";
      break;
    case 4:
      label = "自定义强度";
      break;
  }
  return label;
});
const specificPasswordListEditModalVisible = ref(false);

/**
 * 添加禁止密码
 */
const handleAddSpecificPasswordInputItem = () => {
  passwordStrengthForm.specificPasswordList.push("");
};

/**
 * 删除禁止密码
 */
const handleDeleteSpecificPasswordInputItem = (index: number) => {
  passwordStrengthForm.specificPasswordList.splice(index, 1);
};

/**
 * 密码轮换策略
 */
const forceChangePasswordForm = reactive({
  enableForceChangePassword: false,
  forcedCycle: 1,
  forcedCycleUnit: "MONTH",
  remindCycle: 7,
  remindCycleUnit: "DAY",
});
const forceChangePasswordFormRef = ref(null);
const forceChangePasswordFormRules = {
  forcedCycle: [{ required: true, message: "强制修改密码周期未输入" }],
  remindCycle: [{ required: true, message: "密码到期前提醒周期未输入" }],
};
const handleResetForceChangePasswordForm = () => {
  forceChangePasswordForm.enableForceChangePassword = false;
  forceChangePasswordForm.forcedCycle = 1;
  forceChangePasswordForm.forcedCycleUnit = "MONTH";
  forceChangePasswordForm.remindCycle = 7;
  forceChangePasswordForm.remindCycleUnit = "DAY";
};

/**
 * 策略应用主体
 */
const policyPrincipalForm = reactive({
  type: "USER",
  id: [],
});
const policyPrincipalFormRef = ref(null);
const policyPrincipalFormRules = {
  id: [{ required: true, message: "请选择主体" }],
};
/** 用户列表 */
const userList = reactive([]);
const userSearchKeyword = ref("");

/**
 * 获取全部用户
 */
const handleGetAllUsers = () => {
  searchUser(userSearchKeyword.value, {
    page: -1,
    size: 15,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      userList.length = 0;
      userList.push(...data.list);
    });
  });
};

/** 用户组列表 */
const userGroupList = reactive([]);
const userGroupSearchKeyword = ref("");

/**
 * 获取全部用户组
 */
const handleGetAllUserGroups = () => {
  getUserGroupList({
    page: -1,
    size: 15,
    keyword: userGroupSearchKeyword.value,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      userGroupList.length = 0;
      userGroupList.push(...data.list);
    });
  });
};

/**
 * 选择主体类型变化
 */
const principalSelectChange = (value: any) => {
  policyPrincipalForm.id.length = 0;
  if (value === "USER") {
    handleGetAllUsers();
  }
  if (value === "USER_GROUP") {
    handleGetAllUserGroups();
  }
};

/**
 * 获取密码策略详情
 */
const handleGetPasswordPolicyDetail = async (id: string) => {
  return getPasswordPolicyDdetail(id).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      passwordPolicyName.value = data.name;
      passwordPoliyId.value = data.id;

      // 基础信息
      basicInfoForm.name = data.name;
      basicInfoForm.desc = data.desc;

      // 密码强度
      passwordStrengthForm.passwordStrength = data.passwordStrength;
      passwordStrengthForm.enablePasswordDetection =
        data.enablePasswordDetection;
      passwordStrengthForm.specificPasswordList =
        data.prohibitedPasswordList || [];
      if (passwordStrengthForm.passwordStrength === 4) {
        passwordStrengthForm.minLength = data.minLength;
        passwordStrengthForm.maxLength = data.maxLength;

        passwordStrengthForm.charType = [];
        if (data.requireNumber) {
          passwordStrengthForm.charType.push("NUMBER");
        }
        if (data.requireLowerCase) {
          passwordStrengthForm.charType.push("LOWER_CASE");
        }
        if (data.requireUpperCase) {
          passwordStrengthForm.charType.push("UPPER_CASE");
        }
        if (data.requireSpecialChar) {
          passwordStrengthForm.charType.push("SPECIAL_CHAR");
        }
        if (data.minCharTypeCount === passwordStrengthForm.charType.length) {
          passwordStrengthForm.minCharTypeCount = 0;
        } else {
          passwordStrengthForm.minCharTypeCount = data.minCharTypeCount;
        }
        passwordStrengthForm.prohibitUserInfo = data.prohibitUserInfo;
        passwordStrengthForm.prohibitConsecutiveChar =
          data.prohibitConsecutiveChar;
        passwordStrengthForm.prohibitContainConsecutiveChar =
          data.prohibitContainConsecutiveChar;
        passwordStrengthForm.minConsecutiveCharLength =
          data.minConsecutiveCharLength;
        passwordStrengthForm.prohibitContainRepeatChar =
          data.prohibitContainRepeatChar;
        passwordStrengthForm.minRepeatCharLength = data.minRepeatCharLength;
        passwordStrengthForm.prohibitSpecificPassword =
          data.prohibitSpecificPassword;
        passwordStrengthForm.prohibitSingleChar = data.prohibitSingleChar;
      }

      // 密码轮换策略
      forceChangePasswordForm.enableForceChangePassword =
        data.enableForceChangePassword;
      forceChangePasswordForm.forcedCycle = data.forcedCycle;
      forceChangePasswordForm.forcedCycleUnit = data.forcedCycleUnit;
      forceChangePasswordForm.remindCycle = data.remindCycle;
      forceChangePasswordForm.remindCycleUnit = data.remindCycleUnit;

      // 策略应用主体
      if (data.userIds?.length > 0) {
        policyPrincipalForm.type = "USER";
        policyPrincipalForm.id = data.userIds;
      }
      if (data.userGroupIds?.length > 0) {
        policyPrincipalForm.type = "USER_GROUP";
        policyPrincipalForm.id = data.userGroupIds;
      }
    });
  });
};

/**
 * 提交更新密码策略表单
 */
const handleUpdatePasswordPolicyFormSubmit = async () => {
  let hasError = false;
  updatePasswordPolicyForm.id = passwordPoliyId.value;
  // 基础信息
  await basicInfoFormRef.value.validate((errors) => {
    if (!errors) {
      updatePasswordPolicyForm.name = basicInfoForm.name;
      updatePasswordPolicyForm.desc = basicInfoForm.desc;
    } else {
      hasError = true;
    }
  });

  // 策略应用主体
  if (passwordPolicyName.value !== "默认策略") {
    await policyPrincipalFormRef.value.validate((errors) => {
      if (!errors) {
        if (policyPrincipalForm.type === "USER") {
          updatePasswordPolicyForm.userIds.length = 0;
          updatePasswordPolicyForm.userIds.push(...policyPrincipalForm.id);
        }
        if (policyPrincipalForm.type === "USER_GROUP") {
          updatePasswordPolicyForm.userGroupIds.length = 0;
          updatePasswordPolicyForm.userGroupIds.push(...policyPrincipalForm.id);
        }
      } else {
        hasError = true;
      }
    });
  }

  // 密码强度
  // 是否开启用户登录密码强度检查
  updatePasswordPolicyForm.enablePasswordDetection =
    passwordStrengthForm.enablePasswordDetection;
  updatePasswordPolicyForm.passwordStrength =
    passwordStrengthForm.passwordStrength;
  if (passwordStrengthForm.passwordStrength === 4) {
    await passwordStrengthFormRef.value.validate((errors) => {
      if (!errors) {
        handleSetPasswordStrength();
      } else {
        hasError = true;
      }
    });
  }

  // 密码轮换策略
  // 是否开启强制修改密码
  updatePasswordPolicyForm.enableForceChangePassword =
    forceChangePasswordForm.enableForceChangePassword;
  if (forceChangePasswordForm.enableForceChangePassword) {
    await forceChangePasswordFormRef.value.validate((errors) => {
      if (!errors) {
        // 强制修改密码周期
        updatePasswordPolicyForm.forcedCycle =
          forceChangePasswordForm.forcedCycle;
        updatePasswordPolicyForm.forcedCycleUnit =
          forceChangePasswordForm.forcedCycleUnit;
        // 密码到期提醒周期
        updatePasswordPolicyForm.remindCycle =
          forceChangePasswordForm.remindCycle;
        updatePasswordPolicyForm.remindCycleUnit =
          forceChangePasswordForm.remindCycleUnit;
      } else {
        hasError = true;
      }
    });
  }

  if (!hasError) {
    updatePasswordPolicy(updatePasswordPolicyForm)
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success("保存成功");
          handleBack();
        });
      })
      .catch((err: any) => {
        handleApiError(err, " 更新密码策略");
      });
  }
};

const handleSetPasswordStrength = () => {
  updatePasswordPolicyForm.passwordStrength =
    passwordStrengthForm.passwordStrength;
  // 最小长度
  updatePasswordPolicyForm.minLength = passwordStrengthForm.minLength;
  // 最大长度
  updatePasswordPolicyForm.maxLength = passwordStrengthForm.maxLength;
  // 是否必须包含数字
  if (passwordStrengthForm.charType.includes("NUMBER")) {
    updatePasswordPolicyForm.requireNumber = true;
  }
  // 是否必须包含小写字母
  if (passwordStrengthForm.charType.includes("LOWER_CASE")) {
    updatePasswordPolicyForm.requireLowerCase = true;
  }
  // 是否必须包含大写字母
  if (passwordStrengthForm.charType.includes("UPPER_CASE")) {
    updatePasswordPolicyForm.requireUpperCase = true;
  }
  // 是否必须包含特殊字符
  if (passwordStrengthForm.charType.includes("SPECIAL_CHAR")) {
    updatePasswordPolicyForm.requireSpecialChar = true;
  }
  // 至少需要满足的字符类型数量
  if (passwordStrengthForm.minCharTypeCount === 0) {
    updatePasswordPolicyForm.minCharTypeCount =
      passwordStrengthForm.charType.length;
  } else {
    updatePasswordPolicyForm.minCharTypeCount =
      passwordStrengthForm.minCharTypeCount;
  }
  // 是否禁止包含用户信息
  updatePasswordPolicyForm.prohibitUserInfo =
    passwordStrengthForm.prohibitUserInfo;
  // 是否禁止全部为单一字符
  updatePasswordPolicyForm.prohibitSingleChar =
    passwordStrengthForm.prohibitSingleChar;
  // 是否禁止全部为连续字符
  updatePasswordPolicyForm.prohibitConsecutiveChar =
    passwordStrengthForm.prohibitConsecutiveChar;
  // 是否禁止包含连续字符
  updatePasswordPolicyForm.prohibitContainConsecutiveChar =
    passwordStrengthForm.prohibitContainConsecutiveChar;
  // 禁止包含的连续字符的最小长度
  updatePasswordPolicyForm.minConsecutiveCharLength =
    passwordStrengthForm.minConsecutiveCharLength;
  // 是否禁止包含连续重复字符
  updatePasswordPolicyForm.prohibitContainRepeatChar =
    passwordStrengthForm.prohibitContainRepeatChar;
  // 禁止包含的连续重复字符的最小长度
  updatePasswordPolicyForm.minRepeatCharLength =
    passwordStrengthForm.minRepeatCharLength;
  // 是否禁止使用特定密码
  updatePasswordPolicyForm.prohibitSpecificPassword =
    passwordStrengthForm.prohibitSpecificPassword;
  // 禁止使用的特定密码列表
  updatePasswordPolicyForm.prohibitedPasswordList =
    passwordStrengthForm.specificPasswordList;
};

/**
 * 重置更新密码策略表单
 */
const handleResetUpdatePasswordPolicyForm = () => {
  handleGetPasswordPolicyDetail(passwordPoliyId.value);
  passwordCheckerRef.value.setPassword("");
};

/**
 * 密码检查
 */
const passwordCheckerRef = ref(null);
const checkLoading = ref(false);
const checkRes = reactive({
  valid: false,
  errorMessage: undefined,
  ruleResults: undefined,
});
const handleCheckPassword = (password: string) => {
  handleSetPasswordStrength();
  delete updatePasswordPolicyForm.prohibitUserInfo;
  checkLoading.value = true;
  checkPassword({
    password,
    passwordPolicy: updatePasswordPolicyForm,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        checkRes.valid = data.valid;
        checkRes.errorMessage = data.errorMessage;
        if (data.ruleResults) {
          checkRes.ruleResults = data.ruleResults;
        } else {
          checkRes.ruleResults = [];
        }
        checkLoading.value = false;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "密码检查");
      checkLoading.value = false;
    });
};

const handleResetCheckRes = () => {
  checkRes.valid = false;
  checkRes.errorMessage = undefined;
  checkRes.ruleResults = undefined;
}

export default defineComponent({
  setup() {
    const passwordPolicyId = getQueryString("id");
    passwordPoliyId.value = passwordPolicyId;

    onMounted(async () => {
      await handleGetPasswordPolicyDetail(passwordPolicyId);
      if (policyPrincipalForm.type === "USER") {
        handleGetAllUsers();
      }
      if (policyPrincipalForm.type === "USER_GROUP") {
        handleGetAllUserGroups();
      }
    });

    return {
      handleBack,
      passwordPolicyName,
      basicInfoForm,
      basicInfoFormRef,
      basicInfoFormRules,
      passwordStrengthForm,
      passwordStrengthFormRef,
      passwordStrengthFormRules,
      forceChangePasswordForm,
      forceChangePasswordFormRef,
      forceChangePasswordFormRules,
      policyPrincipalForm,
      policyPrincipalFormRef,
      policyPrincipalFormRules,
      userList,
      userSearchKeyword,
      handleGetAllUsers,
      userGroupList,
      userGroupSearchKeyword,
      handleGetAllUserGroups,
      principalSelectChange,
      passwordStrengthLabel,
      specificPasswordListEditModalVisible,
      handleAddSpecificPasswordInputItem,
      handleDeleteSpecificPasswordInputItem,
      handleUpdatePasswordPolicyFormSubmit,
      handleResetUpdatePasswordPolicyForm,
      passwordCheckerRef,
      checkLoading,
      checkRes,
      handleCheckPassword,
    };
  },
});
