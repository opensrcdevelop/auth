import {checkPassword, createPasswordPolicy} from "@/api/setting";
import {searchUser} from "@/api/user";
import {getUserGroupList} from "@/api/userGroup";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {computed, defineComponent, onMounted, reactive, ref} from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  handleResetCreatePasswordPolicyForm();
  router.back();
};

const createPasswordPolicyForm = reactive({
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
const userListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取用户列表
 */
const handleGetUserList = (page: number = 1, size: number = 15) => {
  searchUser(userSearchKeyword.value, {
    page,
    size,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      if (page == 1) {
        userList.length = 0;
        userList.push(...data.list);
      } else {
        userList.push(...data.list);
      }
      userListPagination.current = data.current;
      userListPagination.total = data.total;
    });
  });
};

/**
 * 搜索用户
 */
const handleSearchUser = () => {
  handleGetUserList(1);
};

/**
 * 加载更多用户
 */
let loadMoreUserLoading = false;
const loadMoreUser = () => {
  if (loadMoreUserLoading) return;
  if (userList.length < userListPagination.total) {
    loadMoreUserLoading = true;
    userListPagination.current++;
    handleGetUserList(userListPagination.current);
    loadMoreUserLoading = false;
  }
};

/** 用户组列表 */
const userGroupList = reactive([]);
const userGroupSearchKeyword = ref("");
const userGroupListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取用户组列表
 */
const handleGetUserGroupList = (page: number = 1, size: number = 15) => {
  getUserGroupList({
    page,
    size,
    keyword: userGroupSearchKeyword.value,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      if (page == 1) {
        userGroupList.length = 0;
        userGroupList.push(...data.list);
      } else {
        userGroupList.push(...data.list);
      }
      userGroupListPagination.current = data.current;
      userGroupListPagination.total = data.total;
    });
  });
};

/**
 * 搜索用户组
 */
const handleSearchUserGroup = () => {
  handleGetUserGroupList(1);
};

/**
 * 加载更多用户组
 */
let loadMoreUserGroupLoading = false;
const loadMoreUserGroup = () => {
  if (loadMoreUserGroupLoading) return;
  if (userGroupList.length < userGroupListPagination.total) {
    loadMoreUserGroupLoading = true;
    userGroupListPagination.current++;
    handleGetUserGroupList(userGroupListPagination.current);
    loadMoreUserGroupLoading = false;
  }
};

/**
 * 选择主体类型变化
 */
const principalSelectChange = (value: any) => {
  policyPrincipalForm.id.length = 0;
  if (value === "USER") {
    handleGetUserList(1);
  }
  if (value === "USER_GROUP") {
    handleGetUserGroupList(1);
  }
};

/**
 * 提交创建密码策略表单
 */
const handleCreatePasswordPolicyFormSubmit = async () => {
  let hasError = false;
  // 基础信息
  await basicInfoFormRef.value.validate((errors) => {
    if (!errors) {
      createPasswordPolicyForm.name = basicInfoForm.name;
      createPasswordPolicyForm.desc = basicInfoForm.desc;
    } else {
      hasError = true;
    }
  });

  // 策略应用主体
  await policyPrincipalFormRef.value.validate((errors) => {
    if (!errors) {
      if (policyPrincipalForm.type === "USER") {
        createPasswordPolicyForm.userIds.length = 0;
        createPasswordPolicyForm.userIds.push(...policyPrincipalForm.id);
      }
      if (policyPrincipalForm.type === "USER_GROUP") {
        createPasswordPolicyForm.userGroupIds.length = 0;
        createPasswordPolicyForm.userGroupIds.push(...policyPrincipalForm.id);
      }
    } else {
      hasError = true;
    }
  });

  // 密码强度
  // 是否开启用户登录密码强度检查
  createPasswordPolicyForm.enablePasswordDetection =
    passwordStrengthForm.enablePasswordDetection;
  createPasswordPolicyForm.passwordStrength =
    passwordStrengthForm.passwordStrength;
  if (passwordStrengthForm.passwordStrength === 4) {
    await passwordStrengthFormRef.value.validate((errors) => {
      handleSetPasswordStrength();
    });
  }

  // 密码轮换策略
  // 是否开启强制修改密码
  createPasswordPolicyForm.enableForceChangePassword =
    forceChangePasswordForm.enableForceChangePassword;
  if (forceChangePasswordForm.enableForceChangePassword) {
    await forceChangePasswordFormRef.value.validate((errors) => {
      if (!errors) {
        // 强制修改密码周期
        createPasswordPolicyForm.forcedCycle =
          forceChangePasswordForm.forcedCycle;
        createPasswordPolicyForm.remindCycle =
          forceChangePasswordForm.remindCycle;
        // 密码到期提醒周期
        createPasswordPolicyForm.remindCycle =
          forceChangePasswordForm.remindCycle;
        createPasswordPolicyForm.remindCycleUnit =
          forceChangePasswordForm.remindCycleUnit;
      } else {
        hasError = true;
      }
    });
  }

  if (!hasError) {
    createPasswordPolicy(createPasswordPolicyForm)
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success("创建成功");
          handleResetCreatePasswordPolicyForm();
        });
      })
      .catch((err: any) => {
        handleApiError(err, "创建密码策略");
      });
  }
};

const handleSetPasswordStrength = () => {
  createPasswordPolicyForm.passwordStrength =
    passwordStrengthForm.passwordStrength;
  // 最小长度
  createPasswordPolicyForm.minLength = passwordStrengthForm.minLength;
  // 最大长度
  createPasswordPolicyForm.maxLength = passwordStrengthForm.maxLength;
  // 是否必须包含数字
  if (passwordStrengthForm.charType.includes("NUMBER")) {
    createPasswordPolicyForm.requireNumber = true;
  }
  // 是否必须包含小写字母
  if (passwordStrengthForm.charType.includes("LOWER_CASE")) {
    createPasswordPolicyForm.requireLowerCase = true;
  }
  // 是否必须包含大写字母
  if (passwordStrengthForm.charType.includes("UPPER_CASE")) {
    createPasswordPolicyForm.requireUpperCase = true;
  }
  // 是否必须包含特殊字符
  if (passwordStrengthForm.charType.includes("SPECIAL_CHAR")) {
    createPasswordPolicyForm.requireSpecialChar = true;
  }
  // 至少需要满足的字符类型数量
  if (passwordStrengthForm.minCharTypeCount === 0) {
    createPasswordPolicyForm.minCharTypeCount =
      passwordStrengthForm.charType.length;
  } else {
    createPasswordPolicyForm.minCharTypeCount =
      passwordStrengthForm.minCharTypeCount;
  }
  // 是否禁止包含用户信息
  createPasswordPolicyForm.prohibitUserInfo =
    passwordStrengthForm.prohibitUserInfo;
  // 是否禁止全部为单一字符
  createPasswordPolicyForm.prohibitSingleChar =
    passwordStrengthForm.prohibitSingleChar;
  // 是否禁止全部为连续字符
  createPasswordPolicyForm.prohibitConsecutiveChar =
    passwordStrengthForm.prohibitConsecutiveChar;
  // 是否禁止包含连续字符
  createPasswordPolicyForm.prohibitContainConsecutiveChar =
    passwordStrengthForm.prohibitContainConsecutiveChar;
  // 禁止包含的连续字符的最小长度
  createPasswordPolicyForm.minConsecutiveCharLength =
    passwordStrengthForm.minConsecutiveCharLength;
  // 是否禁止包含连续重复字符
  createPasswordPolicyForm.prohibitContainRepeatChar =
    passwordStrengthForm.prohibitContainRepeatChar;
  // 禁止包含的连续重复字符的最小长度
  createPasswordPolicyForm.minRepeatCharLength =
    passwordStrengthForm.minRepeatCharLength;
  // 是否禁止使用特定密码
  createPasswordPolicyForm.prohibitSpecificPassword =
    passwordStrengthForm.prohibitSpecificPassword;
  // 禁止使用的特定密码列表
  createPasswordPolicyForm.prohibitedPasswordList =
    passwordStrengthForm.specificPasswordList;
};

/**
 * 重置创建密码策略表单
 */
const handleResetCreatePasswordPolicyForm = () => {
  basicInfoFormRef.value.resetFields();
  policyPrincipalFormRef.value.resetFields();
  passwordStrengthFormRef.value.resetFields();
  forceChangePasswordFormRef.value.resetFields();
};

/**
 * 密码检查
 */
const checkLoading = ref(false);
const checkRes = reactive({
  valid: false,
  errorMessage: undefined,
  ruleResults: undefined,
});
const handleCheckPassword = (password: string) => {
  handleSetPasswordStrength();
  delete createPasswordPolicyForm.prohibitUserInfo;
  checkLoading.value = true;
  checkPassword({
    password,
    passwordPolicy: createPasswordPolicyForm,
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

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetUserList(1);
    });

    return {
      handleBack,
      basicInfoForm,
      basicInfoFormRef,
      basicInfoFormRules,
      passwordStrengthForm,
      passwordStrengthFormRef,
      passwordStrengthFormRules,
      passwordStrengthLabel,
      specificPasswordListEditModalVisible,
      handleAddSpecificPasswordInputItem,
      handleDeleteSpecificPasswordInputItem,
      forceChangePasswordForm,
      forceChangePasswordFormRef,
      forceChangePasswordFormRules,
      policyPrincipalForm,
      policyPrincipalFormRef,
      policyPrincipalFormRules,
      userList,
      userSearchKeyword,
      handleSearchUser,
      loadMoreUser,
      userGroupList,
      userGroupSearchKeyword,
      handleSearchUserGroup,
      loadMoreUserGroup,
      principalSelectChange,
      handleCreatePasswordPolicyFormSubmit,
      handleResetCreatePasswordPolicyForm,
      checkRes,
      checkLoading,
      handleCheckPassword,
    };
  },
});
