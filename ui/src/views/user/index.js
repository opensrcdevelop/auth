var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
import { getUserAttrs, getUserList, removeUser, searchUser, setUserAttrDisplaySeq, updateUserAttr, downloadUserTemplate, exportUsers, importUsers, } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { getEnabledDictData } from "@/api/dict";
import { usePagination } from "@/hooks/usePagination";
var tableColumns = reactive([]);
var sortableColumns = reactive([]);
var selectableColumnsContainerRef = ref();
var selectableColumns = reactive([]);
var selectableColumnSearchKeyword = ref("");
var selectableColumnsPagination = {
    current: 1,
    total: 0,
};
var sortableColumnsLoading = ref(false);
var selectableColumnsLoading = ref(false);
var allUserColumnsForFilter = reactive([]);
/** 字典数据值 */
var allDictDatas = reactive({});
// 用户列表分页
var userListPagination;
/**
 * 获取用户属性
 */
var handlGetUserAttrs = function () {
    getUserAttrs({
        page: 1,
        size: -1,
        onlyDisplay: true,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            sortableColumns.length = 0;
            sortableColumns.push.apply(sortableColumns, data.list);
            tableColumns.length = 0;
            tableColumns.push.apply(tableColumns, sortableColumns);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户属性");
    });
};
/**
 * 获取所有用户属性
 */
var handleGetAllUserAttrs = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserAttrs({
        page: page,
        size: size,
        onlyDisplay: false,
        keyword: selectableColumnSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                selectableColumns.length = 0;
                selectableColumns.push.apply(selectableColumns, data.list);
            }
            else {
                selectableColumns.push.apply(selectableColumns, data.list);
            }
            selectableColumnsPagination.current = data.current;
            selectableColumnsPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户属性");
    });
};
/**
 * 获取所有用户属性
 */
var handleGetAllUserColumnsForFilter = function () {
    getUserAttrs({
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            allUserColumnsForFilter.length = 0;
            allUserColumnsForFilter.push.apply(allUserColumnsForFilter, data.list);
            allUserColumnsForFilter.forEach(function (item) {
                if (item.dataType === "DICT" && item.dictId) {
                    allDictDatas[item.key] = [];
                    handleGetEnabledDictData(item.key, item.dictId);
                }
            });
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户属性");
    });
};
/**
 * 获取启用的字典数据
 */
var handleGetEnabledDictData = function (attrKey, dictId) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, getEnabledDictData(dictId)];
            case 1:
                result = _a.sent();
                handleApiSuccess(result, function (data) {
                    var _a;
                    allDictDatas[attrKey].length = 0;
                    (_a = allDictDatas[attrKey]).push.apply(_a, data);
                });
                return [3 /*break*/, 3];
            case 2:
                err_1 = _a.sent();
                handleApiError(err_1, "获取启用的字典数据");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
/**
 * 加载更多所有用户属性
 */
var loadMoreAllUserAttrsLoading = false;
var loadMoreAllUserAttrs = function () {
    var container = selectableColumnsContainerRef.value;
    // 滚动到底部
    if (container.scrollTop + container.clientHeight >= container.scrollHeight) {
        if (selectableColumns.length < selectableColumnsPagination.total) {
            if (loadMoreAllUserAttrsLoading)
                return;
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
var handleSearchAllUserAttrs = function () {
    handleGetAllUserAttrs(1);
};
/**
 * 取消显示用户属性
 *
 * @param attr 用户属性
 */
var handleUnDisplayUserAttr = function (attr) {
    sortableColumnsLoading.value = true;
    updateUserAttr({
        id: attr.id,
        userLstDisplay: false,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            handlGetUserAttrs();
            handleGetAllUserAttrs();
        });
    })
        .catch(function (err) {
        handleApiError(err, "取消显示用户属性");
    })
        .finally(function () {
        sortableColumnsLoading.value = false;
    });
};
/**
 * 显示用户属性
 *
 * @param attr 用户属性
 */
var handleDisplayUserAttr = function (attr) {
    selectableColumnsLoading.value = true;
    updateUserAttr({
        id: attr.id,
        userLstDisplay: true,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            handlGetUserAttrs();
            handleGetAllUserAttrs();
        });
    })
        .catch(function (err) {
        handleApiError(err, "显示用户属性");
    })
        .finally(function () {
        selectableColumnsLoading.value = false;
    });
};
/**
 * 设置用户属性显示顺序
 */
var handleSetUserAttrDisplaySeq = function () {
    sortableColumnsLoading.value = true;
    setUserAttrDisplaySeq(tableColumns.map(function (item) {
        return {
            id: item.id,
            seq: tableColumns.indexOf(item),
        };
    }))
        .then(function (result) {
        handleApiSuccess(result, function () {
            handlGetUserAttrs();
            handleGetAllUserAttrs();
        });
    })
        .catch(function (err) {
        handleApiError(err, "设置用户属性显示顺序");
    })
        .finally(function () {
        sortableColumnsLoading.value = false;
    });
};
/** column 拖拽排序 S */
var columnDragIndex = 0;
var handleDragStart = function (ev, index) {
    columnDragIndex = index;
    ev.target.classList.add("moveing");
};
var handleDragEnter = function (ev, index) {
    // 如果拖拽的元素和目标元素相同或目标元素为用户名，则不进行任何操作
    if (columnDragIndex !== index && index !== 0) {
        // 移动元素
        var item = sortableColumns.splice(columnDragIndex, 1)[0];
        sortableColumns.splice(index, 0, item);
        columnDragIndex = index;
    }
};
var handleDragOver = function (ev) {
    ev.dataTransfer.dropEffect = "move";
};
var handleDragEnd = function (ev) {
    ev.target.classList.remove("moveing");
    // 同步 tableColumns
    tableColumns.length = 0;
    tableColumns.push.apply(tableColumns, sortableColumns);
    // 设置用户属性显示顺序
    handleSetUserAttrDisplaySeq();
};
/** column 拖拽排序 E */
var userList = reactive([]);
/** 用户列表过滤条件 */
var userListFiltersRef = ref();
var userListFilters = reactive({
    filters: [
        {
            key: undefined,
            dataType: "STRING",
            value: undefined,
            filterType: undefined,
            extFlg: true,
            cascadeDict: undefined,
        },
    ],
});
/**
 * 选择字段，设置字段表单类型
 */
var handleUserColumnsSelectChange = function (value) {
    var column = allUserColumnsForFilter.find(function (item) { return item.key === value; });
    if (column) {
        var filter = userListFilters.filters.find(function (item) { return item.key === column.key; });
        filter.dataType = column.dataType;
        filter.extFlg = column.extFlg;
        filter.cascadeDict = column.cascadeDict;
        filter.value = undefined;
        filter.filterType = undefined;
    }
};
/**
 * 添加用户列表筛选条件
 */
var handleAddUserListFilter = function () {
    userListFilters.filters.push({
        key: undefined,
        dataType: "STRING",
        value: undefined,
        filterType: undefined,
        extFlg: true,
        cascadeDict: undefined,
    });
};
/**
 * 移除用户列表筛选条件
 */
var handleRemoveUserListFilter = function (index) {
    userListFilters.filters.splice(index, 1);
};
/**
 * 过滤用户
 */
var userListFilterd = ref(false);
var handleFilterUser = function () {
    handleGetUserList(1, userListPagination.pageSize);
};
/**
 * 重置用户列表筛选条件
 */
var handleResetFilterUser = function () {
    userListFiltersRef.value.resetFields();
    userListFilters.filters = [
        {
            key: undefined,
            dataType: "STRING",
            value: undefined,
            filterType: undefined,
            extFlg: true,
            cascadeDict: undefined,
        },
    ];
    handleFilterUser();
    handleGetAllUserColumnsForFilter();
};
/**
 * 用户列表筛选条件非显示处理
 */
var handleUserListFilterHide = function () {
    if (!userListFilterd.value) {
        userListFiltersRef.value.resetFields();
        userListFilters.filters = [
            {
                key: undefined,
                dataType: "STRING",
                value: undefined,
                filterType: undefined,
                extFlg: true,
                cascadeDict: undefined,
            },
        ];
    }
};
/**
 * 获取用户列表
 */
var handleGetUserList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    if (userSerachKeyword.value) {
        handleSearchUser(userSerachKeyword.value, page, size);
        return;
    }
    var filters = userListFilters.filters.filter(function (item) { return item.key && item.filterType && item.value; });
    getUserList({
        page: page,
        size: size,
    }, filters)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userList.length = 0;
            userList.push.apply(userList, data.list);
            userListPagination.updatePagination(data.current, data.total, data.size);
            if (filters.length > 0) {
                userListFilterd.value = true;
            }
            else {
                userListFilterd.value = false;
            }
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户列表");
    });
};
/** 用户检索关键字 */
var userSerachKeyword = ref(null);
/**
 * 搜索用户
 *
 * @param username 用户名
 */
var handleSearchUser = function (username, page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    searchUser(username, {
        page: page,
        size: size,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userList.length = 0;
            userList.push.apply(userList, data.list);
            userListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "搜索用户");
    });
};
/**
 * 清空搜索用户
 */
var handleSearchUserClear = function () {
    handleGetUserList();
};
/**
 * 跳转用户详情
 *
 * @param user 用户信息
 */
var handleToUserDetail = function (user) {
    router.push({
        path: "/user/detail",
        query: {
            id: user.userId,
            active_tab: "user_info",
        },
    });
};
/**
 * 跳转创建用户
 */
var handleToCreateUser = function () {
    router.push({
        path: "/user/create",
    });
};
/**
 * 删除账户
 *
 * @param user 用户信息
 */
var handleRemoveUserAccount = function (user) {
    Modal.warning({
        title: "\u786E\u5B9A\u300C".concat(user.username, "\u300D\u7684\u8D26\u6237\u5417\uFF1F"),
        content: "删除后将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeUser(user.userId)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetUserList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除账户");
            });
        },
    });
};
/**
 * 用户字段宽度变化
 */
var userColumnResieTimer = null;
var handleUserColumnResize = function (dataIndex, width) {
    if (userColumnResieTimer) {
        clearTimeout(userColumnResieTimer);
    }
    userColumnResieTimer = setTimeout(function () {
        var column = tableColumns.find(function (item) { return item.key === dataIndex; });
        if (column) {
            updateUserAttr({
                id: column.id,
                displayWidth: width,
            }).catch(function (err) {
                handleApiError(err, "更新用户字段");
            });
        }
    }, 800);
};
// 导入导出相关
var importResultVisible = ref(false);
var importResult = ref({
    successCount: 0,
    failureCount: 0,
    errors: [],
});
// 下载模版
var handleDownloadTemplate = function () { return __awaiter(void 0, void 0, void 0, function () {
    var blob, err_2;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, downloadUserTemplate()];
            case 1:
                blob = (_a.sent());
                downloadBlob(blob, "用户导入模版.xlsx");
                Notification.success("模版下载成功");
                return [3 /*break*/, 3];
            case 2:
                err_2 = _a.sent();
                handleApiError(err_2, "模版下载");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
// 导出数据
var handleExport = function (exportAll) { return __awaiter(void 0, void 0, void 0, function () {
    var filters, blob, date, err_3;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                filters = userListFilters.filters || [];
                return [4 /*yield*/, exportUsers(filters, exportAll)];
            case 1:
                blob = (_a.sent());
                date = new Date().toISOString().slice(0, 10);
                downloadBlob(blob, "\u7528\u6237\u6570\u636E_".concat(date, ".xlsx"));
                Notification.success(exportAll ? "全部数据导出成功" : "当前页导出成功");
                return [3 /*break*/, 3];
            case 2:
                err_3 = _a.sent();
                handleApiError(err_3, "导出");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
// 导入数据
var handleImport = function (file) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_4;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, importUsers(file)];
            case 1:
                result = (_a.sent());
                importResult.value = result;
                importResultVisible.value = true;
                if (result.failureCount === 0) {
                    // 刷新列表
                    handleGetUserList(1, 15);
                }
                return [3 /*break*/, 3];
            case 2:
                err_4 = _a.sent();
                handleApiError(err_4, "导入");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
// 下载 Blob 工具函数
var downloadBlob = function (blob, filename) {
    var url = URL.createObjectURL(blob);
    var link = document.createElement("a");
    link.href = url;
    link.download = filename;
    link.click();
    URL.revokeObjectURL(url);
};
export default defineComponent({
    setup: function () {
        userListPagination = usePagination("userList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetUserList(page, size);
        });
        onMounted(function () {
            handlGetUserAttrs();
            handleGetAllUserAttrs();
            handleGetAllUserColumnsForFilter();
        });
        return {
            tableColumns: tableColumns,
            sortableColumns: sortableColumns,
            handleDragStart: handleDragStart,
            handleDragEnter: handleDragEnter,
            handleDragOver: handleDragOver,
            handleDragEnd: handleDragEnd,
            userList: userList,
            userListPagination: userListPagination,
            handleUnDisplayUserAttr: handleUnDisplayUserAttr,
            selectableColumns: selectableColumns,
            handleDisplayUserAttr: handleDisplayUserAttr,
            sortableColumnsLoading: sortableColumnsLoading,
            selectableColumnsLoading: selectableColumnsLoading,
            userSerachKeyword: userSerachKeyword,
            handleGetUserList: handleGetUserList,
            handleSearchUserClear: handleSearchUserClear,
            handleToUserDetail: handleToUserDetail,
            handleToCreateUser: handleToCreateUser,
            handleRemoveUserAccount: handleRemoveUserAccount,
            selectableColumnsContainerRef: selectableColumnsContainerRef,
            loadMoreAllUserAttrs: loadMoreAllUserAttrs,
            selectableColumnSearchKeyword: selectableColumnSearchKeyword,
            handleSearchAllUserAttrs: handleSearchAllUserAttrs,
            userListFiltersRef: userListFiltersRef,
            userListFilters: userListFilters,
            handleUserColumnsSelectChange: handleUserColumnsSelectChange,
            handleAddUserListFilter: handleAddUserListFilter,
            handleRemoveUserListFilter: handleRemoveUserListFilter,
            handleFilterUser: handleFilterUser,
            handleResetFilterUser: handleResetFilterUser,
            allUserColumnsForFilter: allUserColumnsForFilter,
            allDictDatas: allDictDatas,
            handleUserColumnResize: handleUserColumnResize,
            userListFilterd: userListFilterd,
            handleUserListFilterHide: handleUserListFilterHide,
            // 导入导出
            importResultVisible: importResultVisible,
            importResult: importResult,
            handleDownloadTemplate: handleDownloadTemplate,
            handleExport: handleExport,
            handleImport: handleImport,
        };
    },
});
