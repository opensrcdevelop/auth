import {
    addChildDicts,
    deleteDict,
    getDictList,
    getRelatableDictDataList,
    getSelectableChildDictList,
    removeChildDicts,
} from "@/api/dict";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, reactive, ref} from "vue";

/** 字典列表 */
const dictList = reactive([]);
const dictSerachKeyword = ref(null);
let dictPagination;

/**
 * 获取字典列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetDictList = (page: number = 1, size: number = 15) => {
  getDictList({
    page,
    size,
    keyword: dictSerachKeyword.value,
    queryChildren: true,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dictList.length = 0;
        dictList.push(...data.list);

        dictPagination.updatePagination(data.current, data.total, data.size);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取字典列表");
    });
};

/** 可选子字典列表 */
const selectableChildDictList = reactive([]);
const relatableDictDataList = reactive([]);
const selectableChildDictModalVisible = ref(false);

/** 添加子字典表单 */
const addChildDictsForm = reactive({
  id: undefined,
  children: undefined,
  relatedDictDataIds: undefined,
});
const addChildDictsFormRef = ref();
const addChildDictsFormRules = {
  children: [
    { required: true, message: "请选择子字典", trigger: "blur" },
    {
      validator: (value, cb) => {
        if (value?.length !== addChildDictsForm.relatedDictDataIds?.length) {
          cb("子字典个数必须与所选关联字典数据个数一致");
        } else {
          cb();
        }
      },
      trigger: "blur"
    },
  ],
  relatedDictDataIds: [
    { required: true, message: "请选择关联字典数据", trigger: "blur" },
    {
      validator: (value, cb) => {
        if (value?.length !== addChildDictsForm.children?.length) {
          cb("关联字典数据个数必须与所选子字典个数一致");
        } else {
          cb();
        }
      },
      trigger: "blur"
    },
  ],
};

const handleAddChildDict = async (dictId: string) => {
  try {
    const apiRes1 = await getSelectableChildDictList(dictId);
    handleApiSuccess(apiRes1, (data: any) => {
      selectableChildDictList.length = 0;
      selectableChildDictList.push(...data);

      if (selectableChildDictList.length === 0) {
        Notification.warning("没有可添加的子字典");
      }
    });
  } catch (err) {
    handleApiError(err, "获取可选子字典列表");
    return;
  }

  if (selectableChildDictList?.length === 0) {
    return;
  }

  try {
    const apiRes2 = await getRelatableDictDataList(dictId);
    handleApiSuccess(apiRes2, (data: any) => {
      relatableDictDataList.length = 0;
      relatableDictDataList.push(...data);

      if (relatableDictDataList?.length === 0) {
        Notification.warning("没有可关联的字典数据");
      }
    });
  } catch (err) {
    handleApiError(err, "获取可关联字典数据列表");
    return;
  }

  if (selectableChildDictList?.length > 0 && relatableDictDataList?.length > 0) {
    addChildDictsForm.id = dictId;
    selectableChildDictModalVisible.value = true;
  }
};

/**
 * 添加子字典表单提交
 */
const handleAddChildDictsFormSubmit = async () => {
  const errors = await addChildDictsFormRef.value.validate();
  if (errors) {
    selectableChildDictModalVisible.value = true;
    return;
  }

  const reqData = [];
  for (let i = 0; i < addChildDictsForm.children.length; i++) {
    reqData.push({
      id: addChildDictsForm.id,
      dataId: addChildDictsForm.relatedDictDataIds[i],
      childId: addChildDictsForm.children[i],
    });
  }

  addChildDicts(reqData).then((result: any) => {
    handleApiSuccess(result, () => {
      Notification.success("添加成功");
      handleCloseAddChildDictsModal();
      handleGetDictList();
    });
  });
};

/**
 * 关闭添加子字典弹框
 */
const handleCloseAddChildDictsModal = () => {
  addChildDictsForm.id = undefined;
  addChildDictsForm.children = undefined;
  addChildDictsForm.relatedDictDataIds = undefined;
  addChildDictsFormRef.value.resetFields();

  selectableChildDictModalVisible.value = false;
};

/**
 * 跳转字典详情
 *
 * @param dict 字典
 */
const handleToDictDetail = (dict: any) => {
  router.push({
    path: "/dict/detail",
    query: {
      id: dict.id,
      active_tab: "dict_info",
    },
  });
};

/**
 * 跳转创建字典
 *
 */
const handleToCreateDict = () => {
  router.push({
    path: "/dict/create",
  });
};

/**
 * 删除字典
 */
const handleDeleteDict = (dict: any) => {
  Modal.warning({
    title: `确定删除字典「${dict.name}」吗？`,
    content: "此操作将删除该字典数据关联的字典数据及用户字典，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteDict(dict.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetDictList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除字典");
        });
    },
  });
};

/**
 * 移除子字典
 */
const handleRemoveChildDict = (dict: any) => {
  Modal.warning({
    title: `确定移除子字典「${dict.name}」吗？`,
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeChildDicts({
        id: dict.parentId,
        childId: dict.id,
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("移除成功");
            handleGetDictList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "移除子字典");
        });
    },
  });
};

export default defineComponent({
  setup() {
    dictPagination = usePagination("dictList", ({ page, size }) => {
      handleGetDictList(page, size);
    });

    return {
      dictList,
      dictSerachKeyword,
      dictPagination,
      handleGetDictList,
      handleToDictDetail,
      handleToCreateDict,
      handleDeleteDict,
      selectableChildDictModalVisible,
      selectableChildDictList,
      relatableDictDataList,
      handleAddChildDict,
      addChildDictsForm,
      addChildDictsFormRef,
      addChildDictsFormRules,
      handleAddChildDictsFormSubmit,
      handleCloseAddChildDictsModal,
      handleRemoveChildDict,
    };
  },
});
