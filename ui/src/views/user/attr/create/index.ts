import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createUserAttr } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import dict from "@/views/dict";
import { getDictList } from "@/api/dict";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/**
 * 创建用户字段表单
 */
const createUserColumnFormRef = ref();
const createUserColumnForm = reactive({
  key: undefined,
  name: undefined,
  dataType: "STRING",
  extFlg: true,
  userLstDisplay: false,
  displayWidth: undefined,
  userVisible: true,
  userEditable: true,
  dictId: undefined,
});
const createUserColumnFormRules = {
  key: [{ required: true, message: "字段 key 未填写" }],
  name: [{ required: true, message: "字段名称未填写" }],
  dataType: [{ required: true, message: "表单类型未选择" }],
  extFlg: [{ required: true, message: "字段类型未选择" }],
  userLstDisplay: [{ required: true, message: "是否在用户列表显示未选择" }],
  displayWidth: [
    {
      validator: (value, cb) => {
        if (createUserColumnForm.userLstDisplay && !value) {
          cb("用户列表显示宽度未填写");
        } else {
          cb();
        }
      },
    },
  ],
  userVisible: [{ required: true, message: "个人中心是否可见未选择" }],
  userEditable: [{ required: true, message: "用户是否可编辑未选择" }],
  dictId: [
    {
      validator: (value, cb) => {
        if (createUserColumnForm.dataType === "DICT" && !value) {
          cb("字典类型未选择");
        } else {
          cb();
        }
      },
    },
  ],
};

/** 字典列表 */
const dictList = reactive([]);
const handleGetDicttList = () => {
  getDictList({
    size: -1,
    page: 15,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictList.length = 0;
        dictList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典列表");
    });
};

/**
 * 提交创建用户字段表单
 */
const handleCreateUserColumnFormSubmit = (formData: any) => {
  createUserAttr(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateUserColumnForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建用户字段");
    });
};

/**
 * 表单类型变化
 */
const handleDataTypeChange = (dataType: string) => {
  if (dataType === "DICT") {
    handleGetDicttList();
  }
};

/**
 * 重置创建用户字段表单
 */
const handleResetCreateUserColumnForm = () => {
  createUserColumnFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createUserColumnForm,
      createUserColumnFormRef,
      createUserColumnFormRules,
      handleCreateUserColumnFormSubmit,
      handleResetCreateUserColumnForm,
      dictList,
      handleGetDicttList,
      handleDataTypeChange,
    };
  },
});
