var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
import { batchUpdateTable, getDataSourceConfDetail, getTableList, testDataSourceConn, updateDataSourceConf, } from "@/api/chatbi";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { computed, defineComponent, onMounted, reactive, ref } from "vue";
import { DS_TYPE_LIST } from "../constants";
import { Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";
import TextEditorModal from "../../modal/TextEditorModal.vue";
import MdEditorModal from "../../modal/MdEditorModal.vue";
import TableFieldListDrawer from "../../drawer/TableFieldListDrawer.vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    if (detectRowChanges()) {
        router.back();
    }
};
var activeTab = ref("data_source_info");
/**
 * tab 切换事件
 */
var handleTabChange = function (tabKey) {
    if (!detectRowChanges())
        return;
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
    handleTabInit(tabKey);
};
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = dataSourceId.value; }
    switch (tabKey) {
        case "data_source_info":
            handleGetDataSourceDetail(id);
            break;
        case "table_list":
            handleGetDataSourceDetail(id);
            handleGetTableList(id);
            break;
    }
};
var dataSourceTypeList = DS_TYPE_LIST;
var dataSourceId = ref("");
var dataSourceName = ref("");
/** 数据源信息表单 */
var dataSourceInfoFormRef = ref();
var dataSourceInfoForm = reactive({
    id: undefined,
    name: undefined,
    type: undefined,
    database: undefined,
    schema: undefined,
    host: undefined,
    port: undefined,
    username: undefined,
    password: undefined,
    jdbcParams: undefined,
    desc: undefined,
});
var dataSourceInfoFormRules = {
    name: [{ required: true, message: "数据源名称未填写" }],
    host: [{ required: true, message: "主机地址未填写" }],
    port: [{ required: true, message: "端口号未填写" }],
    username: [{ required: true, message: "用户名未填写" }],
    password: [{ required: true, message: "密码未填写" }],
};
/**
 * 获取数据源详情
 */
var handleGetDataSourceDetail = function (id) {
    getDataSourceConfDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dataSourceId.value = data.id;
            dataSourceName.value = data.name;
            dataSourceInfoForm.id = data.id;
            dataSourceInfoForm.name = data.name;
            dataSourceInfoForm.type = data.type;
            dataSourceInfoForm.database = data.database;
            dataSourceInfoForm.schema = data.schema;
            dataSourceInfoForm.host = data.host;
            dataSourceInfoForm.port = data.port;
            dataSourceInfoForm.username = data.username;
            dataSourceInfoForm.password = data.password;
            dataSourceInfoForm.jdbcParams = data.jdbcParams;
            dataSourceInfoForm.desc = data.desc;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取数据源详情");
    });
};
/**
 * 测试数据源连接
 */
var hanleTestConn = function () {
    dataSourceInfoFormRef.value.validate(function (errors) {
        if (!errors) {
            testDataSourceConn({
                type: dataSourceInfoForm.type,
                database: dataSourceInfoForm.database,
                host: dataSourceInfoForm.host,
                port: dataSourceInfoForm.port,
                username: dataSourceInfoForm.username,
                password: dataSourceInfoForm.password,
            })
                .then(function (result) {
                handleApiSuccess(result, function (data) {
                    if (data.connected) {
                        Notification.success("连接成功");
                    }
                    else {
                        Notification.error("连接失败");
                    }
                });
            })
                .catch(function (err) {
                handleApiError(err, "测试数据源连接");
            });
        }
    });
};
/**
 * 提交数据源信息表单
 */
var handleDataSourceInfoFormSubmit = function (formData) {
    updateDataSourceConf(formData)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            Notification.success("保存成功");
            handleGetDataSourceDetail(dataSourceId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新数据源配置");
    });
};
/**
 * 重置数据源信息表单
 */
var handleResetDataSourceInfoForm = function () {
    dataSourceInfoFormRef.value.resetFields();
    handleGetDataSourceDetail(dataSourceId.value);
};
/** 数据表列表 */
var tableList = reactive([]);
var tableSearchKeyword = ref("");
var tableListPagination;
var handleTableListPageChange = function (page) {
    tableListPagination.handlePageChange(page, detectRowChanges);
};
var handleTableListPageSizeChange = function (size) {
    tableListPagination.handlePageSizeChange(size, detectRowChanges);
};
var handleGetTableList = function (id, page, size) {
    if (id === void 0) { id = dataSourceId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getTableList(id, {
        page: page,
        size: size,
        keyword: tableSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            tableList.length = 0;
            data.list.forEach(function (item) {
                tableList.push(__assign(__assign({}, item), { _isHovering: false, _originalData: JSON.parse(JSON.stringify(item)) }));
            });
            tableListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取数据表列表");
    });
};
/**
 * 判断行数据是否被修改过
 */
var isRowModified = function (rowData) {
    if (!rowData._originalData)
        return false;
    var _isHovering = rowData._isHovering, _originalData = rowData._originalData, currentData = __rest(rowData, ["_isHovering", "_originalData"]);
    return JSON.stringify(currentData) !== JSON.stringify(_originalData);
};
var detectRowChanges = function () {
    if (!saveBtnDisabled.value) {
        return confirm("有未保存的编辑，是否离开当前页面？");
    }
    return true;
};
/**
 * 鼠标悬停
 */
var handleHoverIn = function (rowData) {
    if (isRowModified(rowData)) {
        rowData._isHovering = true;
    }
};
/**
 * 鼠标移出
 */
var handleHoverOut = function (rowData) {
    if (isRowModified(rowData)) {
        rowData._isHovering = false;
    }
};
/**
 * 重置行数据
 */
var handleResetRow = function (record) {
    if (record._originalData) {
        var index = tableList.findIndex(function (item) { return item.id === record.id; });
        if (index !== -1) {
            var originalData = JSON.parse(JSON.stringify(record._originalData));
            originalData._originalData = record._originalData;
            originalData._isHovering = false;
            tableList[index] = originalData;
        }
    }
};
/** 保存按钮禁用状态 */
var saveBtnDisabled = computed(function () {
    if (!tableList)
        return true;
    if (tableList.find(function (item) { return isRowModified(item); })) {
        return false;
    }
    return true;
});
/** 表注释编辑对话框 */
var textEditorModalVisible = ref(false);
var textEditorModalContent = ref("");
var textEditorModalTitle = ref("");
var textEditorModalRecordId = ref("");
/**
 * 打开表注释编辑对话框
 */
var handleOpenTextEditorModal = function (record) {
    textEditorModalContent.value = record.remark;
    textEditorModalVisible.value = true;
    textEditorModalTitle.value = "编辑表注释 - " + record.name;
    textEditorModalRecordId.value = record.id;
};
/**
 * 关闭表注释编辑对话框
 */
var handleCloseTextEditorModal = function () {
    textEditorModalVisible.value = false;
    textEditorModalContent.value = "";
    textEditorModalTitle.value = "";
    textEditorModalRecordId.value = "";
};
/**
 * 表注释编辑对话框确认
 */
var handleTextEditorModalConfirm = function (newContent) {
    var targetRecord = tableList.find(function (item) { return item.id === textEditorModalRecordId.value; });
    if (targetRecord) {
        targetRecord.remark = newContent;
    }
    handleCloseTextEditorModal();
};
/** 表补充信息编辑对话框 */
var mdEditorModalVisible = ref(false);
var mdEditorModalContent = ref("");
var mdEditorModalTitle = ref("");
var mdEditorModalRecordId = ref("");
/**
 * 打开表补充信息编辑对话框
 */
var handleOpenMdEditorModal = function (record) {
    mdEditorModalContent.value = record.additionalInfo;
    mdEditorModalVisible.value = true;
    mdEditorModalTitle.value = "编辑表补充信息 - " + record.name;
    mdEditorModalRecordId.value = record.id;
};
/**
 * 关闭表补充信息编辑对话框
 */
var handleCloseMdEditorModal = function () {
    mdEditorModalVisible.value = false;
    mdEditorModalContent.value = "";
    mdEditorModalTitle.value = "";
    mdEditorModalRecordId.value = "";
};
/**
 * 表补充信息编辑对话框确认
 */
var handleMdEditorModalConfirm = function (newContent) {
    var targetRecord = tableList.find(function (item) { return item.id === mdEditorModalRecordId.value; });
    if (targetRecord) {
        targetRecord.additionalInfo = newContent;
    }
    handleCloseMdEditorModal();
};
/**
 * 保存表数据
 */
var handleSaveTableList = function () {
    var list = tableList
        .filter(function (item) { return isRowModified(item); })
        .map(function (item) {
        var _isHovering = item._isHovering, _originalData = item._originalData, currentData = __rest(item, ["_isHovering", "_originalData"]);
        return currentData;
    });
    batchUpdateTable({
        list: list,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetTableList(dataSourceId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "批量更新表");
    });
};
/** 表字段列表 Drawer */
var tableFieldListDrawerVisible = ref(false);
var tableFieldListDrawerTitle = ref("");
var tableFieldListDrawerTableId = ref("");
var handleOpenTableFieldListDrawer = function (record) {
    tableFieldListDrawerVisible.value = true;
    tableFieldListDrawerTitle.value = "表字段列表 - " + record.name;
    tableFieldListDrawerTableId.value = record.id;
};
var handleCloseTableFieldListDrawer = function () {
    tableFieldListDrawerVisible.value = false;
    tableFieldListDrawerTitle.value = "";
    tableFieldListDrawerTableId.value = "";
};
export default defineComponent({
    components: {
        TextEditorModal: TextEditorModal,
        MdEditorModal: MdEditorModal,
        TableFieldListDrawer: TableFieldListDrawer,
    },
    setup: function () {
        var dataSourceId = getQueryString("id");
        tableListPagination = usePagination("tableList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetTableList(dataSourceId, page, size);
        });
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "data_source_info";
            handleTabInit(activeTab.value, dataSourceId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            dataSourceTypeList: dataSourceTypeList,
            dataSourceId: dataSourceId,
            dataSourceName: dataSourceName,
            dataSourceInfoFormRef: dataSourceInfoFormRef,
            dataSourceInfoForm: dataSourceInfoForm,
            dataSourceInfoFormRules: dataSourceInfoFormRules,
            hanleTestConn: hanleTestConn,
            handleDataSourceInfoFormSubmit: handleDataSourceInfoFormSubmit,
            handleResetDataSourceInfoForm: handleResetDataSourceInfoForm,
            tableList: tableList,
            tableListPagination: tableListPagination,
            tableSearchKeyword: tableSearchKeyword,
            handleGetTableList: handleGetTableList,
            handleTableListPageChange: handleTableListPageChange,
            handleTableListPageSizeChange: handleTableListPageSizeChange,
            isRowModified: isRowModified,
            handleHoverIn: handleHoverIn,
            handleHoverOut: handleHoverOut,
            handleResetRow: handleResetRow,
            saveBtnDisabled: saveBtnDisabled,
            textEditorModalVisible: textEditorModalVisible,
            textEditorModalTitle: textEditorModalTitle,
            textEditorModalContent: textEditorModalContent,
            handleOpenTextEditorModal: handleOpenTextEditorModal,
            handleCloseTextEditorModal: handleCloseTextEditorModal,
            handleTextEditorModalConfirm: handleTextEditorModalConfirm,
            mdEditorModalVisible: mdEditorModalVisible,
            mdEditorModalTitle: mdEditorModalTitle,
            mdEditorModalContent: mdEditorModalContent,
            handleOpenMdEditorModal: handleOpenMdEditorModal,
            handleCloseMdEditorModal: handleCloseMdEditorModal,
            handleMdEditorModalConfirm: handleMdEditorModalConfirm,
            handleSaveTableList: handleSaveTableList,
            tableFieldListDrawerVisible: tableFieldListDrawerVisible,
            tableFieldListDrawerTitle: tableFieldListDrawerTitle,
            tableFieldListDrawerTableId: tableFieldListDrawerTableId,
            handleOpenTableFieldListDrawer: handleOpenTableFieldListDrawer,
            handleCloseTableFieldListDrawer: handleCloseTableFieldListDrawer
        };
    },
});
