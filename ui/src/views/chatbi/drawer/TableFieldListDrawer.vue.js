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
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
import { batchUpdateTableField, getTableFieldList } from "@/api/chatbi";
import { usePagination } from "@/hooks/usePagination";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { computed, reactive, ref, watch } from "vue";
import TextEditorModal from "../modal/TextEditorModal.vue";
import MdEditorModal from "../modal/MdEditorModal.vue";
import { Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
var globalVariables = useGlobalVariablesStore();
var props = withDefaults(defineProps(), {
    visible: false,
    title: "",
    tableId: "",
});
var emits = defineEmits();
var handleClose = function () {
    if (detectRowChanges()) {
        tableFieldList.length = 0;
        emits("close");
    }
};
watch(function () { return props.visible; }, function (val) {
    if (val) {
        handleGetTableFieldList(1, 15);
    }
});
/** 表字段列表 */
var tableFieldList = reactive([]);
var tableFieldSearchKeyword = ref("");
var tableFieldListPagination = usePagination("tableFieldList", function (_a) {
    var page = _a.page, size = _a.size;
    handleGetTableFieldList(page, size);
});
var handleTableFieldListPageChange = function (page) {
    tableFieldListPagination.handlePageChange(page, detectRowChanges);
};
var handleTableFieldListPageSizeChange = function (size) {
    tableFieldListPagination.handlePageSizeChange(size, detectRowChanges);
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
        var index = tableFieldList.findIndex(function (item) { return item.id === record.id; });
        if (index !== -1) {
            var originalData = JSON.parse(JSON.stringify(record._originalData));
            originalData._originalData = record._originalData;
            originalData._isHovering = false;
            tableFieldList[index] = originalData;
        }
    }
};
/** 保存按钮禁用状态 */
var saveBtnDisabled = computed(function () {
    if (!tableFieldList)
        return true;
    if (tableFieldList.find(function (item) { return isRowModified(item); })) {
        return false;
    }
    return true;
});
/**
 * 获取表字段列表
 */
var handleGetTableFieldList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    if (!props.tableId)
        return;
    getTableFieldList(props.tableId, {
        page: page,
        size: size,
        keyword: tableFieldSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            tableFieldList.length = 0;
            data.list.forEach(function (item) {
                tableFieldList.push(__assign(__assign({}, item), { _isHovering: false, _originalData: JSON.parse(JSON.stringify(item)) }));
            });
            tableFieldListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取表字段列表");
    });
};
/** 表字段注释编辑对话框 */
var textEditorModalVisible = ref(false);
var textEditorModalContent = ref("");
var textEditorModalTitle = ref("");
var textEditorModalRecordId = ref("");
/**
 * 打开表字段注释编辑对话框
 */
var handleOpenTextEditorModal = function (record) {
    textEditorModalContent.value = record.remark;
    textEditorModalVisible.value = true;
    textEditorModalTitle.value = "编辑字段注释 - " + record.name;
    textEditorModalRecordId.value = record.id;
};
/**
 * 关闭表字段注释编辑对话框
 */
var handleCloseTextEditorModal = function () {
    textEditorModalVisible.value = false;
    textEditorModalContent.value = "";
    textEditorModalTitle.value = "";
    textEditorModalRecordId.value = "";
};
/**
 * 表字段注释编辑对话框确认
 */
var handleTextEditorModalConfirm = function (newContent) {
    var targetRecord = tableFieldList.find(function (item) { return item.id === textEditorModalRecordId.value; });
    if (targetRecord) {
        targetRecord.remark = newContent;
    }
    handleCloseTextEditorModal();
};
/** 表字段补充信息编辑对话框 */
var mdEditorModalVisible = ref(false);
var mdEditorModalContent = ref("");
var mdEditorModalTitle = ref("");
var mdEditorModalRecordId = ref("");
/**
 * 打开表字段补充信息编辑对话框
 */
var handleOpenMdEditorModal = function (record) {
    mdEditorModalContent.value = record.additionalInfo;
    mdEditorModalVisible.value = true;
    mdEditorModalTitle.value = "编辑字段补充信息 - " + record.name;
    mdEditorModalRecordId.value = record.id;
};
/**
 * 关闭表字段补充信息编辑对话框
 */
var handleCloseMdEditorModal = function () {
    mdEditorModalVisible.value = false;
    mdEditorModalContent.value = "";
    mdEditorModalTitle.value = "";
    mdEditorModalRecordId.value = "";
};
/**
 * 表字段补充信息编辑对话框确认
 */
var handleMdEditorModalConfirm = function (newContent) {
    var targetRecord = tableFieldList.find(function (item) { return item.id === mdEditorModalRecordId.value; });
    if (targetRecord) {
        targetRecord.additionalInfo = newContent;
    }
    handleCloseMdEditorModal();
};
/**
 * 保存表字段数据
 */
var handleSaveTableFieldList = function () {
    var list = tableFieldList
        .filter(function (item) { return isRowModified(item); })
        .map(function (item) {
        var _isHovering = item._isHovering, _originalData = item._originalData, currentData = __rest(item, ["_isHovering", "_originalData"]);
        return currentData;
    });
    batchUpdateTableField({
        list: list,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetTableFieldList();
        });
    })
        .catch(function (err) {
        handleApiError(err, "批量更新表字段");
    });
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    visible: false,
    title: "",
    tableId: "",
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
var __VLS_0 = {}.ADrawer;
/** @type {[typeof __VLS_components.ADrawer, typeof __VLS_components.aDrawer, typeof __VLS_components.ADrawer, typeof __VLS_components.aDrawer, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.visible), footer: (false), width: "100%" })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.visible), footer: (false), width: "100%" })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onCancel: (__VLS_ctx.handleClose)
};
__VLS_3.slots.default;
{
    var __VLS_thisSlot = __VLS_3.slots.title;
    (__VLS_ctx.title);
}
var __VLS_8 = {}.ASpin;
/** @type {[typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign({ class: "spin" }, { tip: "处理中，请稍后...", loading: (__VLS_ctx.globalVariables.apiLoading) })));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign({ class: "spin" }, { tip: "处理中，请稍后...", loading: (__VLS_ctx.globalVariables.apiLoading) })], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "list-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "list-header" }));
var __VLS_12 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入字段名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.tableFieldSearchKeyword) })));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入字段名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.tableFieldSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16;
var __VLS_17;
var __VLS_18;
var __VLS_19 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTableFieldList(1, 15);
    }
};
var __VLS_20 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTableFieldList(1, 15);
    }
};
var __VLS_21 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTableFieldList(1, 15);
    }
};
var __VLS_15;
var __VLS_22 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22(__assign({ 'onClick': {} }, { type: "primary", disabled: (__VLS_ctx.saveBtnDisabled) })));
var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", disabled: (__VLS_ctx.saveBtnDisabled) })], __VLS_functionalComponentArgsRest(__VLS_23), false));
var __VLS_26;
var __VLS_27;
var __VLS_28;
var __VLS_29 = {
    onClick: (__VLS_ctx.handleSaveTableFieldList)
};
__VLS_25.slots.default;
var __VLS_25;
var __VLS_30 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.tableFieldList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.tableFieldListPagination.pagination) })));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.tableFieldList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.tableFieldListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_31), false));
var __VLS_34;
var __VLS_35;
var __VLS_36;
var __VLS_37 = {
    onPageChange: (__VLS_ctx.handleTableFieldListPageChange)
};
var __VLS_38 = {
    onPageSizeChange: (__VLS_ctx.handleTableFieldListPageSizeChange)
};
__VLS_33.slots.default;
{
    var __VLS_thisSlot = __VLS_33.slots.columns;
    var __VLS_39 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39({
        title: "编辑状态",
        width: (100),
    }));
    var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([{
            title: "编辑状态",
            width: (100),
        }], __VLS_functionalComponentArgsRest(__VLS_40), false));
    __VLS_42.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_42.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "edit-status-container" }));
        var __VLS_43 = {}.transition;
        /** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
        // @ts-ignore
        var __VLS_44 = __VLS_asFunctionalComponent(__VLS_43, new __VLS_43({
            name: "fade",
        }));
        var __VLS_45 = __VLS_44.apply(void 0, __spreadArray([{
                name: "fade",
            }], __VLS_functionalComponentArgsRest(__VLS_44), false));
        __VLS_46.slots.default;
        if (!record_1._isHovering || !__VLS_ctx.isRowModified(record_1)) {
            var __VLS_47 = {}.ATag;
            /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
            // @ts-ignore
            var __VLS_48 = __VLS_asFunctionalComponent(__VLS_47, new __VLS_47(__assign(__assign({ 'onMouseenter': {} }, { class: "status-tag" }), { class: ({ modified: __VLS_ctx.isRowModified(record_1) }) })));
            var __VLS_49 = __VLS_48.apply(void 0, __spreadArray([__assign(__assign({ 'onMouseenter': {} }, { class: "status-tag" }), { class: ({ modified: __VLS_ctx.isRowModified(record_1) }) })], __VLS_functionalComponentArgsRest(__VLS_48), false));
            var __VLS_51 = void 0;
            var __VLS_52 = void 0;
            var __VLS_53 = void 0;
            var __VLS_54 = {
                onMouseenter: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(!record_1._isHovering || !__VLS_ctx.isRowModified(record_1)))
                        return;
                    __VLS_ctx.handleHoverIn(record_1);
                }
            };
            __VLS_50.slots.default;
            (__VLS_ctx.isRowModified(record_1) ? "已编辑" : "未编辑");
            var __VLS_50;
        }
        var __VLS_46;
        var __VLS_55 = {}.transition;
        /** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
        // @ts-ignore
        var __VLS_56 = __VLS_asFunctionalComponent(__VLS_55, new __VLS_55({
            name: "fade",
        }));
        var __VLS_57 = __VLS_56.apply(void 0, __spreadArray([{
                name: "fade",
            }], __VLS_functionalComponentArgsRest(__VLS_56), false));
        __VLS_58.slots.default;
        if (record_1._isHovering && __VLS_ctx.isRowModified(record_1)) {
            var __VLS_59 = {}.AButton;
            /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
            // @ts-ignore
            var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59(__assign(__assign(__assign({ 'onMouseleave': {} }, { 'onClick': {} }), { class: "status-btn" }), { type: "outline", size: "mini", status: "warning" })));
            var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onMouseleave': {} }, { 'onClick': {} }), { class: "status-btn" }), { type: "outline", size: "mini", status: "warning" })], __VLS_functionalComponentArgsRest(__VLS_60), false));
            var __VLS_63 = void 0;
            var __VLS_64 = void 0;
            var __VLS_65 = void 0;
            var __VLS_66 = {
                onMouseleave: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(record_1._isHovering && __VLS_ctx.isRowModified(record_1)))
                        return;
                    __VLS_ctx.handleHoverOut(record_1);
                }
            };
            var __VLS_67 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(record_1._isHovering && __VLS_ctx.isRowModified(record_1)))
                        return;
                    __VLS_ctx.handleResetRow(record_1);
                }
            };
            __VLS_62.slots.default;
            var __VLS_62;
        }
        var __VLS_58;
    }
    var __VLS_42;
    var __VLS_68 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
        title: "字段名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([{
            title: "字段名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_69), false));
    __VLS_71.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_71.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.name);
    }
    var __VLS_71;
    var __VLS_72 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
        title: "字段类型",
        width: (100),
    }));
    var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([{
            title: "字段类型",
            width: (100),
        }], __VLS_functionalComponentArgsRest(__VLS_73), false));
    __VLS_75.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_75.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.type);
    }
    var __VLS_75;
    var __VLS_76 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
        title: "字段注释",
    }));
    var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([{
            title: "字段注释",
        }], __VLS_functionalComponentArgsRest(__VLS_77), false));
    __VLS_79.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_79.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        var __VLS_80 = {}.ASpace;
        /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
        // @ts-ignore
        var __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({}));
        var __VLS_82 = __VLS_81.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_81), false));
        __VLS_83.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "ellipsis-text" }));
        (record_2.remark ? record_2.remark : "-");
        var __VLS_84 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84(__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })));
        var __VLS_86 = __VLS_85.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_85), false));
        var __VLS_88 = void 0;
        var __VLS_89 = void 0;
        var __VLS_90 = void 0;
        var __VLS_91 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleOpenTextEditorModal(record_2);
            }
        };
        __VLS_87.slots.default;
        {
            var __VLS_thisSlot_5 = __VLS_87.slots.icon;
            var __VLS_92 = {}.IconEdit;
            /** @type {[typeof __VLS_components.IconEdit, typeof __VLS_components.iconEdit, ]} */ ;
            // @ts-ignore
            var __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({}));
            var __VLS_94 = __VLS_93.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_93), false));
        }
        var __VLS_87;
        var __VLS_83;
    }
    var __VLS_79;
    var __VLS_96 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
        title: "补充信息",
    }));
    var __VLS_98 = __VLS_97.apply(void 0, __spreadArray([{
            title: "补充信息",
        }], __VLS_functionalComponentArgsRest(__VLS_97), false));
    __VLS_99.slots.default;
    {
        var __VLS_thisSlot_6 = __VLS_99.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_6)[0].record;
        var __VLS_100 = {}.ASpace;
        /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
        // @ts-ignore
        var __VLS_101 = __VLS_asFunctionalComponent(__VLS_100, new __VLS_100({}));
        var __VLS_102 = __VLS_101.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_101), false));
        __VLS_103.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "ellipsis-text" }));
        (record_3.additionalInfo ? record_3.additionalInfo : "-");
        var __VLS_104 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104(__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })));
        var __VLS_106 = __VLS_105.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_105), false));
        var __VLS_108 = void 0;
        var __VLS_109 = void 0;
        var __VLS_110 = void 0;
        var __VLS_111 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleOpenMdEditorModal(record_3);
            }
        };
        __VLS_107.slots.default;
        {
            var __VLS_thisSlot_7 = __VLS_107.slots.icon;
            var __VLS_112 = {}.IconEdit;
            /** @type {[typeof __VLS_components.IconEdit, typeof __VLS_components.iconEdit, ]} */ ;
            // @ts-ignore
            var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({}));
            var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_113), false));
        }
        var __VLS_107;
        var __VLS_103;
    }
    var __VLS_99;
    var __VLS_116 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
        title: "是否使用",
        width: (100),
    }));
    var __VLS_118 = __VLS_117.apply(void 0, __spreadArray([{
            title: "是否使用",
            width: (100),
        }], __VLS_functionalComponentArgsRest(__VLS_117), false));
    __VLS_119.slots.default;
    {
        var __VLS_thisSlot_8 = __VLS_119.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_8)[0].record;
        var __VLS_120 = {}.ASwitch;
        /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
        // @ts-ignore
        var __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
            type: "round",
            size: "small",
            modelValue: (record.toUse),
        }));
        var __VLS_122 = __VLS_121.apply(void 0, __spreadArray([{
                type: "round",
                size: "small",
                modelValue: (record.toUse),
            }], __VLS_functionalComponentArgsRest(__VLS_121), false));
    }
    var __VLS_119;
}
var __VLS_33;
var __VLS_11;
var __VLS_3;
/** @type {[typeof TextEditorModal, ]} */ ;
// @ts-ignore
var __VLS_124 = __VLS_asFunctionalComponent(TextEditorModal, new TextEditorModal(__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.textEditorModalVisible), content: (__VLS_ctx.textEditorModalContent), title: (__VLS_ctx.textEditorModalTitle) })));
var __VLS_125 = __VLS_124.apply(void 0, __spreadArray([__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.textEditorModalVisible), content: (__VLS_ctx.textEditorModalContent), title: (__VLS_ctx.textEditorModalTitle) })], __VLS_functionalComponentArgsRest(__VLS_124), false));
var __VLS_127;
var __VLS_128;
var __VLS_129;
var __VLS_130 = {
    onClose: (__VLS_ctx.handleCloseTextEditorModal)
};
var __VLS_131 = {
    onConfirm: (__VLS_ctx.handleTextEditorModalConfirm)
};
var __VLS_126;
/** @type {[typeof MdEditorModal, ]} */ ;
// @ts-ignore
var __VLS_132 = __VLS_asFunctionalComponent(MdEditorModal, new MdEditorModal(__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.mdEditorModalVisible), content: (__VLS_ctx.mdEditorModalContent), title: (__VLS_ctx.mdEditorModalTitle) })));
var __VLS_133 = __VLS_132.apply(void 0, __spreadArray([__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.mdEditorModalVisible), content: (__VLS_ctx.mdEditorModalContent), title: (__VLS_ctx.mdEditorModalTitle) })], __VLS_functionalComponentArgsRest(__VLS_132), false));
var __VLS_135;
var __VLS_136;
var __VLS_137;
var __VLS_138 = {
    onClose: (__VLS_ctx.handleCloseMdEditorModal)
};
var __VLS_139 = {
    onConfirm: (__VLS_ctx.handleMdEditorModalConfirm)
};
var __VLS_134;
/** @type {__VLS_StyleScopedClasses['spin']} */ ;
/** @type {__VLS_StyleScopedClasses['list-container']} */ ;
/** @type {__VLS_StyleScopedClasses['list-header']} */ ;
/** @type {__VLS_StyleScopedClasses['edit-status-container']} */ ;
/** @type {__VLS_StyleScopedClasses['status-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['modified']} */ ;
/** @type {__VLS_StyleScopedClasses['status-btn']} */ ;
/** @type {__VLS_StyleScopedClasses['ellipsis-text']} */ ;
/** @type {__VLS_StyleScopedClasses['ellipsis-text']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            TextEditorModal: TextEditorModal,
            MdEditorModal: MdEditorModal,
            globalVariables: globalVariables,
            handleClose: handleClose,
            tableFieldList: tableFieldList,
            tableFieldSearchKeyword: tableFieldSearchKeyword,
            tableFieldListPagination: tableFieldListPagination,
            handleTableFieldListPageChange: handleTableFieldListPageChange,
            handleTableFieldListPageSizeChange: handleTableFieldListPageSizeChange,
            isRowModified: isRowModified,
            handleHoverIn: handleHoverIn,
            handleHoverOut: handleHoverOut,
            handleResetRow: handleResetRow,
            saveBtnDisabled: saveBtnDisabled,
            handleGetTableFieldList: handleGetTableFieldList,
            textEditorModalVisible: textEditorModalVisible,
            textEditorModalContent: textEditorModalContent,
            textEditorModalTitle: textEditorModalTitle,
            handleOpenTextEditorModal: handleOpenTextEditorModal,
            handleCloseTextEditorModal: handleCloseTextEditorModal,
            handleTextEditorModalConfirm: handleTextEditorModalConfirm,
            mdEditorModalVisible: mdEditorModalVisible,
            mdEditorModalContent: mdEditorModalContent,
            mdEditorModalTitle: mdEditorModalTitle,
            handleOpenMdEditorModal: handleOpenMdEditorModal,
            handleCloseMdEditorModal: handleCloseMdEditorModal,
            handleMdEditorModalConfirm: handleMdEditorModalConfirm,
            handleSaveTableFieldList: handleSaveTableFieldList,
        };
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
