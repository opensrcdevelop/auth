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
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
import { deleteDataSourceConf, getDataSourceConfList, syncTable, updateDataSourceConf, } from "@/api/chatbi";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { reactive, ref } from "vue";
/** 数据源列表 */
var dataSourceList = reactive([]);
var dataSourceSearchKeyword = ref(null);
var dataSourceListPagination = usePagination("dataSourceList", function (_a) {
    var page = _a.page, size = _a.size;
    handleGetDataSourceList(page, size);
});
/**
 * 获取数据源列表
 */
var handleGetDataSourceList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getDataSourceConfList({
        page: page,
        size: size,
        keyword: dataSourceSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dataSourceList.length = 0;
            dataSourceList.push.apply(dataSourceList, data.list);
            dataSourceListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取数据源列表");
    });
};
/**
 * 跳转到数据源详情
 */
var handleToDataSourceDetail = function (dataSource) {
    router.push({
        path: "/chatbi/datasource/detail",
        query: {
            id: dataSource.id,
            active_tab: "data_source_info",
        },
    });
};
/**
 * 跳转到创建数据源
 */
var handleToCreateDataSource = function () {
    router.push({
        path: "/chatbi/datasource/create",
    });
};
/**
 * 同步表
 */
var handleSyncTable = function (dataSource) {
    syncTable(dataSource.id)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("操作成功");
        });
    })
        .catch(function (err) {
        handleApiError(err, "同步表");
    });
};
/**
 * 更新数据源启用状态
 */
var handleUpdateDataSourceState = function (dataSource) {
    updateDataSourceConf({
        id: dataSource.id,
        enabled: dataSource.enabled,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("更新数据源启用状态成功");
            handleGetDataSourceList();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新数据源启用状态");
    });
};
/**
 * 删除数据源
 */
var handleDeleteDataSource = function (dataSource) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u6570\u636E\u6E90\u300C".concat(dataSource.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该数据源及包含的所有表，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteDataSourceConf(dataSource.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetDataSourceList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除资源组");
            });
        },
    });
};
/**
 * 初始化
 */
var init = function () {
    handleGetDataSourceList(dataSourceListPagination.pagination.current, dataSourceListPagination.pagination.pageSize);
};
var __VLS_exposed = {
    init: init,
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "header" }));
var __VLS_0 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入数据源名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.dataSourceSearchKeyword) })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入数据源名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.dataSourceSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetDataSourceList(1, 15);
    }
};
var __VLS_8 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetDataSourceList(1, 15);
    }
};
var __VLS_9 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetDataSourceList(1, 15);
    }
};
var __VLS_3;
var __VLS_10 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_11 = __VLS_asFunctionalComponent(__VLS_10, new __VLS_10(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_12 = __VLS_11.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_11), false));
var __VLS_14;
var __VLS_15;
var __VLS_16;
var __VLS_17 = {
    onClick: (__VLS_ctx.handleToCreateDataSource)
};
__VLS_13.slots.default;
var __VLS_13;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "datasource-list" }));
var __VLS_18 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.dataSourceList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.dataSourceListPagination.pagination) })));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.dataSourceList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.dataSourceListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_22;
var __VLS_23;
var __VLS_24;
var __VLS_25 = {
    onPageChange: (__VLS_ctx.dataSourceListPagination.handlePageChange)
};
var __VLS_26 = {
    onPageSizeChange: (__VLS_ctx.dataSourceListPagination.handlePageSizeChange)
};
__VLS_21.slots.default;
{
    var __VLS_thisSlot = __VLS_21.slots.columns;
    var __VLS_27 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_28 = __VLS_asFunctionalComponent(__VLS_27, new __VLS_27({
        title: "数据源名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_29 = __VLS_28.apply(void 0, __spreadArray([{
            title: "数据源名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_28), false));
    __VLS_30.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_30.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToDataSourceDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.name);
    }
    var __VLS_30;
    var __VLS_31 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_32 = __VLS_asFunctionalComponent(__VLS_31, new __VLS_31({
        title: "数据源类型",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_33 = __VLS_32.apply(void 0, __spreadArray([{
            title: "数据源类型",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_32), false));
    __VLS_34.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_34.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.type);
    }
    var __VLS_34;
    var __VLS_35 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_36 = __VLS_asFunctionalComponent(__VLS_35, new __VLS_35({
        title: "数据源描述",
        ellipsis: true,
        tooltip: true,
    }));
    var __VLS_37 = __VLS_36.apply(void 0, __spreadArray([{
            title: "数据源描述",
            ellipsis: true,
            tooltip: true,
        }], __VLS_functionalComponentArgsRest(__VLS_36), false));
    __VLS_38.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_38.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.desc ? record.desc : "-");
    }
    var __VLS_38;
    var __VLS_39 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39({
        title: "是否启用",
    }));
    var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([{
            title: "是否启用",
        }], __VLS_functionalComponentArgsRest(__VLS_40), false));
    __VLS_42.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_42.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        var __VLS_43 = {}.ASwitch;
        /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
        // @ts-ignore
        var __VLS_44 = __VLS_asFunctionalComponent(__VLS_43, new __VLS_43(__assign({ 'onChange': {} }, { type: "round", size: "small", modelValue: (record_2.enabled) })));
        var __VLS_45 = __VLS_44.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { type: "round", size: "small", modelValue: (record_2.enabled) })], __VLS_functionalComponentArgsRest(__VLS_44), false));
        var __VLS_47 = void 0;
        var __VLS_48 = void 0;
        var __VLS_49 = void 0;
        var __VLS_50 = {
            onChange: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleUpdateDataSourceState(record_2);
            }
        };
        var __VLS_46;
    }
    var __VLS_42;
    var __VLS_51 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_52 = __VLS_asFunctionalComponent(__VLS_51, new __VLS_51({
        title: "表数量",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_53 = __VLS_52.apply(void 0, __spreadArray([{
            title: "表数量",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_52), false));
    __VLS_54.slots.default;
    {
        var __VLS_thisSlot_5 = __VLS_54.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_5)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.tableCount ? record.tableCount : "-");
    }
    var __VLS_54;
    var __VLS_55 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_56 = __VLS_asFunctionalComponent(__VLS_55, new __VLS_55({
        title: "最后同步表时间",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_57 = __VLS_56.apply(void 0, __spreadArray([{
            title: "最后同步表时间",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_56), false));
    __VLS_58.slots.default;
    {
        var __VLS_thisSlot_6 = __VLS_58.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_6)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.lastSyncTableTime ? record.lastSyncTableTime : "-");
    }
    var __VLS_58;
    var __VLS_59 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59({
        title: "同步表次数",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([{
            title: "同步表次数",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_60), false));
    __VLS_62.slots.default;
    {
        var __VLS_thisSlot_7 = __VLS_62.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_7)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.syncTableCount ? record.syncTableCount : "-");
    }
    var __VLS_62;
    var __VLS_63 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_64 = __VLS_asFunctionalComponent(__VLS_63, new __VLS_63({
        title: "操作",
        fixed: "right",
        width: (60),
    }));
    var __VLS_65 = __VLS_64.apply(void 0, __spreadArray([{
            title: "操作",
            fixed: "right",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_64), false));
    __VLS_66.slots.default;
    {
        var __VLS_thisSlot_8 = __VLS_66.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_8)[0].record;
        var __VLS_67 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_68 = __VLS_asFunctionalComponent(__VLS_67, new __VLS_67({}));
        var __VLS_69 = __VLS_68.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_68), false));
        __VLS_70.slots.default;
        var __VLS_71 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_72 = __VLS_asFunctionalComponent(__VLS_71, new __VLS_71({
            type: "text",
        }));
        var __VLS_73 = __VLS_72.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_72), false));
        __VLS_74.slots.default;
        {
            var __VLS_thisSlot_9 = __VLS_74.slots.icon;
            var __VLS_75 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_76 = __VLS_asFunctionalComponent(__VLS_75, new __VLS_75({}));
            var __VLS_77 = __VLS_76.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_76), false));
        }
        var __VLS_74;
        {
            var __VLS_thisSlot_10 = __VLS_70.slots.content;
            var __VLS_79 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_80 = __VLS_asFunctionalComponent(__VLS_79, new __VLS_79(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_81 = __VLS_80.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_80), false));
            var __VLS_83 = void 0;
            var __VLS_84 = void 0;
            var __VLS_85 = void 0;
            var __VLS_86 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleSyncTable(record_3);
                }
            };
            __VLS_82.slots.default;
            {
                var __VLS_thisSlot_11 = __VLS_82.slots.icon;
                var __VLS_87 = {}.IconSync;
                /** @type {[typeof __VLS_components.IconSync, typeof __VLS_components.iconSync, ]} */ ;
                // @ts-ignore
                var __VLS_88 = __VLS_asFunctionalComponent(__VLS_87, new __VLS_87({}));
                var __VLS_89 = __VLS_88.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_88), false));
            }
            var __VLS_82;
            var __VLS_91 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_92 = __VLS_asFunctionalComponent(__VLS_91, new __VLS_91(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_93 = __VLS_92.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_92), false));
            var __VLS_95 = void 0;
            var __VLS_96 = void 0;
            var __VLS_97 = void 0;
            var __VLS_98 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeleteDataSource(record_3);
                }
            };
            __VLS_94.slots.default;
            {
                var __VLS_thisSlot_12 = __VLS_94.slots.icon;
                var __VLS_99 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_100 = __VLS_asFunctionalComponent(__VLS_99, new __VLS_99({}));
                var __VLS_101 = __VLS_100.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_100), false));
            }
            var __VLS_94;
        }
        var __VLS_70;
    }
    var __VLS_66;
}
var __VLS_21;
/** @type {__VLS_StyleScopedClasses['header']} */ ;
/** @type {__VLS_StyleScopedClasses['datasource-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            dataSourceList: dataSourceList,
            dataSourceSearchKeyword: dataSourceSearchKeyword,
            dataSourceListPagination: dataSourceListPagination,
            handleGetDataSourceList: handleGetDataSourceList,
            handleToDataSourceDetail: handleToDataSourceDetail,
            handleToCreateDataSource: handleToCreateDataSource,
            handleSyncTable: handleSyncTable,
            handleUpdateDataSourceState: handleUpdateDataSourceState,
            handleDeleteDataSource: handleDeleteDataSource,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
});
; /* PartiallyEnd: #4569/main.vue */
