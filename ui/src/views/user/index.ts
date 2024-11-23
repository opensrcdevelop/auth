import {
  getUserAttrs,
  getUserList,
  removeUser,
  searchUser,
  setUserAttrDisplaySeq,
  updateUserAttr,
} from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { getEnabledDictData } from "@/api/dict";

const tableColumns = reactive([]);
const sortableColumns = reactive([]);

const selectableColumnsContainerRef = ref();
const selectableColumns = reactive([]);
const selectableColumnSearchKeyword = ref("");
const selectableColumnsPagination = {
  current: 1,
  total: 0,
};

const sortableColumnsLoading = ref(false);
const selectableColumnsLoading = ref(false);

const allUserColumnsForFilter = reactive([]);

/** 字典数据值 */
const allDictDatas = reactive({});

/**
 * 获取用户属性
 */
const handlGetUserAttrs = () => {
  getUserAttrs({
    page: 1,
    size: -1,
    onlyDisplay: true,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        sortableColumns.length = 0;
        sortableColumns.push(...data.list);

        tableColumns.length = 0;
        tableColumns.push(...sortableColumns);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户属性");
    });
};

/**
 * 获取所有用户属性
 */
const handleGetAllUserAttrs = (page: number = 1, size: number = 15) => {
  getUserAttrs({
    page,
    size,
    onlyDisplay: false,
    keyword: selectableColumnSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          selectableColumns.length = 0;
          selectableColumns.push(...data.list);
        } else {
          selectableColumns.push(...data.list);
        }
        selectableColumnsPagination.current = data.current;
        selectableColumnsPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户属性");
    });
};

/**
 * 获取所有用户属性
 */
const handleGetAllUserColumnsForFilter = () => {
  getUserAttrs({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        allUserColumnsForFilter.length = 0;
        allUserColumnsForFilter.push(...data.list);

        selectableColumns.forEach((item: any) => {
          if (item.dataType === "DICT" && item.dictId) {
            allDictDatas[item.key] = [];
            handleGetEnabledDictData(item.key, item.dictId);
          }
        });
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户属性");
    });
};

/**
 * 获取启用的字典数据
 */
const handleGetEnabledDictData = async (attrKey: string, dictId: string) => {
  try {
    const result = await getEnabledDictData(dictId);
    handleApiSuccess(result, (data: any) => {
      allDictDatas[attrKey].length = 0;
      allDictDatas[attrKey].push(...data);
    });
  } catch (err: any) {
    handleApiError(err, "获取启用的字典数据");
  }
};

/**
 * 加载更多所有用户属性
 */
let loadMoreAllUserAttrsLoading = false;
const loadMoreAllUserAttrs = () => {
  const container = selectableColumnsContainerRef.value;
  // 滚动到底部
  if (container.scrollTop + container.clientHeight >= container.scrollHeight) {
    if (selectableColumns.length < selectableColumnsPagination.total) {
      if (loadMoreAllUserAttrsLoading) return;
      loadMoreAllUserAttrsLoading = true;
      selectableColumnsPagination.current++;
      handleGetAllUserAttrs(selectableColumnsPagination.current);
      loadMoreAllUserAttrsLoading = false;
    }
  }
};

/**
 * 搜索所有用户属性
 */
const handleSearchAllUserAttrs = () => {
  handleGetAllUserAttrs(1);
};

/**
 * 取消显示用户属性
 *
 * @param attr 用户属性
 */
const handleUnDisplayUserAttr = (attr: any) => {
  sortableColumnsLoading.value = true;
  updateUserAttr({
    id: attr.id,
    userLstDisplay: false,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        handlGetUserAttrs();
        handleGetAllUserAttrs();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "取消显示用户属性");
    })
    .finally(() => {
      sortableColumnsLoading.value = false;
    });
};

/**
 * 显示用户属性
 *
 * @param attr 用户属性
 */
const handleDisplayUserAttr = (attr: any) => {
  selectableColumnsLoading.value = true;
  updateUserAttr({
    id: attr.id,
    userLstDisplay: true,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        handlGetUserAttrs();
        handleGetAllUserAttrs();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "显示用户属性");
    })
    .finally(() => {
      selectableColumnsLoading.value = false;
    });
};

/**
 * 设置用户属性显示顺序
 */
const handleSetUserAttrDisplaySeq = () => {
  sortableColumnsLoading.value = true;
  setUserAttrDisplaySeq(
    tableColumns.map((item) => {
      return {
        id: item.id,
        seq: tableColumns.indexOf(item),
      };
    })
  )
    .then((result: any) => {
      handleApiSuccess(result, () => {
        handlGetUserAttrs();
        handleGetAllUserAttrs();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "设置用户属性显示顺序");
    })
    .finally(() => {
      sortableColumnsLoading.value = false;
    });
};

/** column 拖拽排序 S */
let columnDragIndex = 0;
const handleDragStart = (ev, index) => {
  columnDragIndex = index;
  ev.target.classList.add("moveing");
};
const handleDragEnter = (ev, index) => {
  // 如果拖拽的元素和目标元素相同或目标元素为用户名，则不进行任何操作
  if (columnDragIndex !== index && index !== 0) {
    // 移动元素
    const [item] = sortableColumns.splice(columnDragIndex, 1);
    sortableColumns.splice(index, 0, item);
    columnDragIndex = index;
  }
};
const handleDragOver = (ev) => {
  ev.dataTransfer.dropEffect = "move";
};
const handleDragEnd = (ev) => {
  ev.target.classList.remove("moveing");

  // 同步 tableColumns
  tableColumns.length = 0;
  tableColumns.push(...sortableColumns);

  // 设置用户属性显示顺序
  handleSetUserAttrDisplaySeq();
};
/** column 拖拽排序 E */

const userList = reactive([]);
/** 用户列表过滤条件 */
const userListFiltersRef = ref();
const userListFilters = reactive({
  filters: [
    {
      key: undefined,
      dataType: "STRING",
      value: undefined,
      filterType: undefined,
      extFlg: true,
    },
  ],
});

/**
 * 选择字段，设置字段表单类型
 */
const handleUserColumnsSelectChange = (value: any) => {
  const column = allUserColumnsForFilter.find((item) => item.key === value);
  if (column) {
    const filter = userListFilters.filters.find(
      (item) => item.key === column.key
    );
    filter.dataType = column.dataType;
    filter.extFlg = column.extFlg;
    filter.value = undefined;
    filter.filterType = undefined;
  }
};

/**
 * 添加用户列表筛选条件
 */
const handleAddUserListFilter = () => {
  userListFilters.filters.push({
    key: undefined,
    dataType: "STRING",
    value: undefined,
    filterType: undefined,
    extFlg: true,
  });
};

/**
 * 移除用户列表筛选条件
 */
const handleRemoveUserListFilter = (index: number) => {
  userListFilters.filters.splice(index, 1);
};

/**
 * 过滤用户
 */
const handleFilterUser = () => {
  handleGetUserList(1, userListPagination.pageSize);
};

/**
 * 重置用户列表筛选条件
 */
const handleResetFilterUser = () => {
  userListFiltersRef.value.resetFields();
  userListFilters.filters = [
    {
      key: undefined,
      dataType: "STRING",
      value: undefined,
      filterType: undefined,
      extFlg: true,
    },
  ];
  handleFilterUser();
};

/**
 * 获取用户列表
 */
const handleGetUserList = (page: number = 1, size: number = 15) => {
  const filters = userListFilters.filters.filter(
    (item) => item.key && item.filterType
  );

  getUserList(
    {
      page,
      size,
    },
    filters
  )
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userList.length = 0;
        userList.push(...data.list);

        userListPagination.current = data.current;
        userListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户列表");
    });
};

const userListPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 页数变化
 *
 * @param page 页数
 */
const handlePageChange = (page: number) => {
  userListPagination.current = page;
  handleGetUserList(page, userListPagination.pageSize);
};

/**
 * 分页大小变化
 *
 * @param page 分页大小
 */
const handlePageSizeChange = (size: number) => {
  userListPagination.pageSize = size;
  handleGetUserList(1, size);
};

/** 用户检索关键字 */
const userSerachKeyword = ref("");

/**
 * 搜索用户
 *
 * @param username 用户名
 */
const handleSearchUser = (username: string) => {
  searchUser(username, {
    page: 1,
    size: 15,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userList.length = 0;
        userList.push(...data.list);

        userListPagination.current = data.current;
        userListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "搜索用户");
    });
};

/**
 * 清空搜索用户
 */
const handleSearchUserClear = () => {
  handleGetUserList();
};

/**
 * 跳转用户详情
 *
 * @param user 用户信息
 */
const handleToUserDetail = (user: any) => {
  router.push({
    path: "/user/detail",
    query: {
      id: user.userId,
    },
  });
};

/**
 * 跳转创建用户
 */
const handleToCreateUser = () => {
  router.push({
    path: "/user/create",
  });
};

/**
 * 删除账户
 *
 * @param user 用户信息
 */
const handleRemoveUserAccount = (user: any) => {
  Modal.warning({
    title: `确定「${user.username}」的账户吗？`,
    content: "删除后将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeUser(user.userId)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetUserList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除账户");
        });
    },
  });
};

/**
 * 用户字段宽度变化
 */
let userColumnResieTimer: any = null;
const handleUserColumnResize = (dataIndex: string, width: number) => {
  if (userColumnResieTimer) {
    clearTimeout(userColumnResieTimer);
  }

  userColumnResieTimer = setTimeout(() => {
    const column = tableColumns.find((item) => item.key === dataIndex);
    if (column) {
      updateUserAttr({
        id: column.id,
        displayWidth: width
      }).catch((err: any) => {
        handleApiError(err, "更新用户字段");
      })
    }
  }, 800);
}

export default defineComponent({
  setup() {
    onMounted(() => {
      handlGetUserAttrs();
      handleGetAllUserAttrs();
      handleGetUserList();
      handleGetAllUserColumnsForFilter();
    });

    return {
      tableColumns,
      sortableColumns,
      handleDragStart,
      handleDragEnter,
      handleDragOver,
      handleDragEnd,
      userList,
      userListPagination,
      handlePageChange,
      handlePageSizeChange,
      handleUnDisplayUserAttr,
      selectableColumns,
      handleDisplayUserAttr,
      sortableColumnsLoading,
      selectableColumnsLoading,
      userSerachKeyword,
      handleSearchUser,
      handleSearchUserClear,
      handleToUserDetail,
      handleToCreateUser,
      handleRemoveUserAccount,
      selectableColumnsContainerRef,
      loadMoreAllUserAttrs,
      selectableColumnSearchKeyword,
      handleSearchAllUserAttrs,
      userListFiltersRef,
      userListFilters,
      handleUserColumnsSelectChange,
      handleAddUserListFilter,
      handleRemoveUserListFilter,
      handleFilterUser,
      handleResetFilterUser,
      allUserColumnsForFilter,
      allDictDatas,
      handleUserColumnResize
    };
  },
});
