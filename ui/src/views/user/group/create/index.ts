import {defineComponent, reactive, ref} from "vue";
import router from "@/router";
import {createUserGroup} from "@/api/userGroup";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import UserGroupConditions from "../components/UserGroupConditions.vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/**
 * 创建用户组表单
 */
const createUserGroupForm = reactive({
  name: "",
  code: "",
  type: "STATIC",
  desc: "",
  conditions: {
    filters: [
      {
        key: undefined,
        dataType: "STRING",
        value: undefined,
        filterType: undefined,
        extFlg: undefined,
      },
    ],
    conjunction: "AND",
    groups: [],
  },
});
const createUserGroupFormRef = ref();
const createUserGroupFormRules = {
  name: [{ required: true, message: "用户组名称未填写" }],
  code: [
    { required: true, message: "用户组标识未填写" },
    {
      validator: (value, cb) => {
        if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
          cb("只允许包含英文字母、数字、下划线_、横线-");
        } else {
          cb();
        }
      },
    },
  ],
  type: [{ required: true, message: "用户组类型未选择" }],
};
const userGroupConditionsRef = ref();

/**
 * 重置创建用户组表单
 */
const handleResetCreateUserGroupForm = () => {
  createUserGroupFormRef.value.resetFields();
};

/**
 * 提交创建用户组表单
 *
 * @param formData 创建用户组表单
 */
const handleCreateUserGroupFormSubmit = async () => {
  const validateRes1 = await createUserGroupFormRef.value.validate();
  let validateRes2 = true;
  if (userGroupConditionsRef.value) {
    validateRes2 = await userGroupConditionsRef.value.validate();
  }
  
  if (validateRes1 !== undefined || !validateRes2) {
    return;
  }

  if (createUserGroupForm.type === "STATIC") {
    createUserGroupForm.conditions = undefined;
  }

  createUserGroup(createUserGroupForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateUserGroupForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建用户组");
    });
};

const handleUserGroupTypeChange = (val: string) => {
  if (val === "DYNAMIC") {
    createUserGroupForm.conditions = {
      filters: [
        {
          key: undefined,
          dataType: "STRING",
          value: undefined,
          filterType: undefined,
          extFlg: undefined,
        },
      ],
      conjunction: "AND",
      groups: [],
    };
  }
};

export default defineComponent({
  components: {
    UserGroupConditions,
  },
  setup() {
    return {
      handleBack,
      createUserGroupForm,
      createUserGroupFormRef,
      createUserGroupFormRules,
      handleResetCreateUserGroupForm,
      handleCreateUserGroupFormSubmit,
      userGroupConditionsRef,
      handleUserGroupTypeChange,
    };
  },
});
