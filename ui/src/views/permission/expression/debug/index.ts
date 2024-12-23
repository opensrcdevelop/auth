import { debugPermissionExp } from "@/api/permission";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 权限表达式 */
const permissionExp = reactive({
  name: undefined,
  expression: undefined,
});

/** 调试权限表达式表单 */
const debugPermissionExpFormRef = ref();
const debugPermissionExpForm = reactive({
  expressionId: undefined,
  context: undefined,
});
const debugPermissionExpFormRules = {
  context: {
    validator: (value, cb) => {
      if (value) {
        try {
          const valueObj = JSON.parse(value);
          if (typeof valueObj !== "object") {
            cb("上下文 JSON 字符串必须是一个对象");
          }
        } catch (err: any) {
          cb("请检查 JSON 格式是否正确");
        }
      } else {
        cb();
      }
    },
  },
};

/**
 * 调试权限表达式
 */
const handleDebugPermissionExpFormSubmit = (formData: any) => {
  debugPermissionExp({
    expressionId: formData.expressionId,
    context: formData.context ? JSON.parse(formData.context) : {},
  })
    .then((result) => {
      handleApiSuccess(result, (data: any) => {
        showDebugResult(data);
      });
    })
    .catch((err) => {
      handleApiError(err, "调试限制条件");
    });
};

/**
 * 重置调试权限表达式表单
 */
const handleResetDebugPermissionExpForm = () => {
  debugPermissionExpFormRef.value.resetFields();
  debugPermissionExpForm.context = undefined;
};

/**
 * 显示调试结果
 */
const showDebugResult = (result: any) => {
  if (!result.isSuccess) {
    Modal.error({
      title: "调试运行失败",
      content: `错误消息：${result.executeRes}`,
    });
  } else {
    if (result.executeRes) {
      Modal.success({
        title: "调试运行成功",
        content: `限制条件【${permissionExp.name}】通过`,
      });
    } else {
      Modal.warning({
        title: "调试运行成功",
        content: `限制条件【${permissionExp.name}】未通过`,
      });
    }
  }
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const globalVariables = useGlobalVariablesStore().getData();
      permissionExp.name = globalVariables.permissionExp.name;
      permissionExp.expression = globalVariables.permissionExp.expression;
      debugPermissionExpForm.expressionId = globalVariables.permissionExp.id;
    });

    return {
      handleBack,
      permissionExp,
      debugPermissionExpFormRef,
      debugPermissionExpForm,
      debugPermissionExpFormRules,
      handleDebugPermissionExpFormSubmit,
      handleResetDebugPermissionExpForm,
    };
  },
});
