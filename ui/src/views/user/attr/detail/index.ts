import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { getUserAttrDetail, updateUserAttr } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { useRoute } from "vue-router";
import { Notification } from "@arco-design/web-vue";
import { getDictList } from "@/api/dict";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("user_column_info");

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

const userColumnId = ref("");
const userColumnName = ref("");

/** 用户字段信息表单 */
const userColumnInfoFormRef = ref();
const userColumnInfoForm = reactive({
  id: undefined,
  name: undefined,
  key: undefined,
  dataType: undefined,
  extFlg: undefined,
  userLstDisplay: undefined,
  displayWidth: undefined,
  userVisible: undefined,
  userEditable: undefined,
  dictId: undefined,
});
const userColumnInfoFormRules = {
  name: [{ required: true, message: "字段名称未填写" }],
  userLstDisplay: [{ required: true, message: "是否在用户列表显示未选择" }],
  userVisible: [{ required: true, message: "个人中心是否可见未选择" }],
  userEditable: [{ required: true, message: "用户是否可编辑未选择" }],
};

/**
 * 获取用户字段详情
 *
 * @param id 用户字段id
 */
const handleGetUserColumnDetail = (id: string) => {
  getUserAttrDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userColumnId.value = data.id;
        userColumnName.value = data.name;

        userColumnInfoForm.id = data.id;
        userColumnInfoForm.name = data.name;
        userColumnInfoForm.key = data.key;
        userColumnInfoForm.dataType = data.dataType;
        userColumnInfoForm.extFlg = data.extFlg;
        userColumnInfoForm.userLstDisplay = data.userLstDisplay;
        userColumnInfoForm.displayWidth = data.displayWidth;
        userColumnInfoForm.userVisible = data.userVisible;
        userColumnInfoForm.userEditable = data.userEditable;
        userColumnInfoForm.dictId = data.dictId;

        if (data.dataType === "DICT" && data.dictId) {
          handleGetDicttList();
        }
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户字段详情");
    });
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
 * 重置用户字段信息表单
 */
const handleResetUserColumnInfoForm = () => {
  userColumnInfoFormRef.value.resetFields();
  handleGetUserColumnDetail(userColumnId.value);
};

/**
 * 提交用户字段信息表单
 */
const handleUserColumnInfoFormSubmit = () => {
  updateUserAttr(userColumnInfoForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetUserColumnDetail(userColumnId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户字段");
    });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      const userColumnId = route.query.id as string;

      handleGetUserColumnDetail(userColumnId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      userColumnId,
      userColumnName,
      userColumnInfoFormRef,
      userColumnInfoForm,
      userColumnInfoFormRules,
      handleResetUserColumnInfoForm,
      handleUserColumnInfoFormSubmit,
      dictList
    };
  },
});
