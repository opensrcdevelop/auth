import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import {
  deleteDictData,
  getDictDataList,
  getDictDetail,
  updateDict,
} from "@/api/dict";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { useRoute } from "vue-router";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";

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
};

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

        handleGetDictDataList();
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
const dictDataPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取字典数据列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetDictDataList = (page: number = 1, size: number = 15) => {
  getDictDataList(dictId.value, {
    page,
    size,
    keyword: dictDataSerachKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictDataList.length = 0;
        dictDataList.push(...data.list);

        dictDataPagination.current = data.current;
        dictDataPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典数据列表");
    });
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  dictDataPagination.current = page;
  handleGetDictDataList(page, dictDataPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  dictDataPagination.pageSize = size;
  handleGetDictDataList(1, size);
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
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      const dictId = route.query.id as string;
      handleGetDictDetail(dictId);
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
      handlePageChange,
      handlePageSizeChange,
      handleToDictDataDetail,
      handleToCreateDictData,
      handleDeleteDictData,
    };
  },
});
