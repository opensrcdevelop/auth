import {defineComponent, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {deleteDictData, getDictDataList, getDictDetail, updateDict,} from "@/api/dict";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import {usePagination} from "@/hooks/usePagination";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("dict_info");

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
  handleTabInit(tabKey);
};

const handleTabInit = (tabKey: string, id: string = dictId.value) => {
  switch (tabKey) {
    case "dict_info":
      handleGetDictDetail(id);
      break;
    case "dict_data":
      handleGetDictDetail(id);
      handleGetDictDataList(id);
      break;
  }
}

/** 字典信息表单 */
const dictInfoFormRef = ref();
const dictInfoForm = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
  desc: undefined,
});
const dictInfoFormRules = {
  name: [{ required: true, message: "字典名称未填写" }],
  code: [{ required: true, message: "字典标识未填写" }],
};

const dictId = ref("");
const dictName = ref("");

/**
 * 获取字典详情
 *
 * @param id 字典 ID
 */
const handleGetDictDetail = (id: string) => {
  getDictDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictId.value = data.id;
        dictName.value = data.name;

        dictInfoForm.id = data.id;
        dictInfoForm.name = data.name;
        dictInfoForm.code = data.code;
        dictInfoForm.desc = data.desc;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典详情");
    });
};

/**
 * 重置字典信息表单
 */
const handleResetDictInfoForm = () => {
  dictInfoFormRef.value.resetFields();
  handleGetDictDetail(dictId.value);
};

/**
 * 提交字典信息表单
 *
 * @param formData 字典信息表单
 */
const handleDictInfoFormSubmit = (formData: any) => {
  updateDict(formData)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        Notification.success("保存成功");
        handleGetDictDetail(dictId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新字典信息");
    });
};

/** 字典数据列表 */
const dictDataSerachKeyword = ref(null);
const dictDataList = reactive([]);
let dictDataPagination;

/**
 * 获取字典数据列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetDictDataList = (
  id: string = dictId.value,
  page: number = 1,
  size: number = 15
) => {
  getDictDataList(id, {
    page,
    size,
    keyword: dictDataSerachKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictDataList.length = 0;
        dictDataList.push(...data.list);

        dictDataPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典数据列表");
    });
};

/**
 * 跳转字典数据详情
 *
 * @param dictData 字典数据
 */
const handleToDictDataDetail = (dictData: any) => {
  router.push({
    path: "/dict/data/detail",
    query: {
      id: dictData.id,
      active_tab: "dict_data_info",
    },
  });
};

/**
 * 跳转创建字典数据
 */
const handleToCreateDictData = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.dictId = dictId.value;
  globalVariables.saveData();
  router.push({
    path: "/dict/data/create",
  });
};

/**
 * 删除字典数据
 */
const handleDeleteDictData = (dicData: any) => {
  Modal.warning({
    title: `确定删除字典数据「${dicData.label}」吗？`,
    content: "此操作将删除该字典数据关联的用户字段值，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteDictData(dicData.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetDictDataList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除字典数据");
        });
    },
  });
};

export default defineComponent({
  setup() {
    const dictId = getQueryString("id");
    const tab = getQueryString("active_tab");
    dictDataPagination = usePagination(
      `${dictId}_dictDataList`,
      ({ page, size }) => {
        if (tab === "dict_data") {
          handleGetDictDataList(dictId, page, size);
        }
      }
    );

    onMounted(() => {
      activeTab.value = tab || "dict_info";
      handleTabInit(activeTab.value, dictId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      dictInfoFormRef,
      dictInfoForm,
      dictInfoFormRules,
      dictId,
      dictName,
      handleResetDictInfoForm,
      handleDictInfoFormSubmit,
      dictDataSerachKeyword,
      dictDataList,
      dictDataPagination,
      handleGetDictDataList,
      handleToDictDataDetail,
      handleToCreateDictData,
      handleDeleteDictData,
    };
  },
});
