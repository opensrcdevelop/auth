import { getDictDataDetail, updateDictData } from "@/api/dict";
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

const activeTab = ref("dict_data_info");

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

/** 字典数据信息表单 */
const dictDataInfoFormRef = ref();
const dictDataInfoForm = reactive({
  dictId: undefined,
  id: undefined,
  label: undefined,
  value: undefined,
  enable: undefined,
  displaySeq: undefined,
});
const dictDataInfoFormRules = {
  label: [{ required: true, message: "数据标签未填写" }],
  value: [{ required: true, message: "数据值未填写" }],
};

const dictDataId = ref("");
const dictDataLabel = ref("");

/**
 * 获取字典数据详情
 *
 * @param id 字典数据 ID
 */
const handleGetDictDatatDetail = (id: string) => {
  getDictDataDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictDataId.value = data.id;
        dictDataLabel.value = data.label;

        dictDataInfoForm.dictId = data.dictId;
        dictDataInfoForm.id = data.id;
        dictDataInfoForm.label = data.label;
        dictDataInfoForm.value = data.value;
        dictDataInfoForm.enable = data.enable;
        dictDataInfoForm.displaySeq = data.displaySeq;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典数据详情");
    });
};

/**
 * 重置字典数据信息表单
 */
const handleResetDictDataInfoForm = () => {
  dictDataInfoFormRef.value.resetFields();
  handleGetDictDatatDetail(dictDataId.value);
};

/**
 * 提交字典数据信息表单
 *
 * @param formData 字典数据信息表单
 */
const handleDictDataInfoFormSubmit = (formData: any) => {
  updateDictData(formData)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        Notification.success("保存成功");
        handleGetDictDatatDetail(dictDataId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新字典数据信息");
    });
};


export default defineComponent({
  setup() {
    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "dict_data_info";
      handleGetDictDatatDetail(getQueryString("id"));
    })

    return {
      handleBack,
      activeTab,
      handleTabChange,
      dictDataInfoFormRef,
      dictDataInfoForm,
      dictDataInfoFormRules,
      dictDataId,
      dictDataLabel,
      handleDictDataInfoFormSubmit,
      handleResetDictDataInfoForm
    };
  },
});
