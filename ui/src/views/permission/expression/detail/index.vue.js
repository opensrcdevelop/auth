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
import detailTs from "./index";
export default detailTs;
debugger; /* PartiallyEnd: #3632/script.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
var __VLS_0 = {}.PageHeader;
/** @type {[typeof __VLS_components.PageHeader, typeof __VLS_components.pageHeader, typeof __VLS_components.PageHeader, typeof __VLS_components.pageHeader, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onBack': {} })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onBack': {} })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onBack: (__VLS_ctx.handleBack)
};
__VLS_3.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "detail-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "title" }));
(__VLS_ctx.permissionExpName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.permissionExpId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.permissionExpId),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
if (!__VLS_ctx.permissionExpInfoForm.templateId) {
    var __VLS_12 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onClick': {} }, { type: "primary" })));
    var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_13), false));
    var __VLS_16 = void 0;
    var __VLS_17 = void 0;
    var __VLS_18 = void 0;
    var __VLS_19 = {
        onClick: (__VLS_ctx.handleOpenDebugDrawer)
    };
    __VLS_15.slots.default;
    var __VLS_15;
}
var __VLS_20 = {}.ATabs;
/** @type {[typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, typeof __VLS_components.ATabs, typeof __VLS_components.aTabs, ]} */ ;
// @ts-ignore
var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20(__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })));
var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { activeKey: (__VLS_ctx.activeTab) })], __VLS_functionalComponentArgsRest(__VLS_21), false));
var __VLS_24;
var __VLS_25;
var __VLS_26;
var __VLS_27 = {
    onChange: (__VLS_ctx.handleTabChange)
};
__VLS_23.slots.default;
var __VLS_28 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
    key: "condition_info",
    title: "限制条件信息",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        key: "condition_info",
        title: "限制条件信息",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_32 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.permissionExpInfoForm), ref: "permissionExpInfoFormRef", rules: (__VLS_ctx.permissionExpInfoFormRules), layout: "vertical" })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.permissionExpInfoForm), ref: "permissionExpInfoFormRef", rules: (__VLS_ctx.permissionExpInfoFormRules), layout: "vertical" })], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_36;
var __VLS_37;
var __VLS_38;
var __VLS_39 = {
    onSubmitSuccess: (__VLS_ctx.handlePermissionExpInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.permissionExpInfoFormRef} */ ;
var __VLS_40 = {};
__VLS_35.slots.default;
var __VLS_42 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    field: "name",
    label: "限制条件名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        field: "name",
        label: "限制条件名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    modelValue: (__VLS_ctx.permissionExpInfoForm.name),
    placeholder: "请输入限制条件名称",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.permissionExpInfoForm.name),
        placeholder: "请输入限制条件名称",
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
var __VLS_45;
if (__VLS_ctx.permissionExpInfoForm.templateId) {
    var __VLS_50 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
        field: "templateId",
        label: "限制条件模板",
    }));
    var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
            field: "templateId",
            label: "限制条件模板",
        }], __VLS_functionalComponentArgsRest(__VLS_51), false));
    __VLS_53.slots.default;
    var __VLS_54 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
        placeholder: "请选择限制条件模板",
        disabled: true,
        modelValue: (__VLS_ctx.permissionExpInfoForm.templateId),
    }));
    var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
            placeholder: "请选择限制条件模板",
            disabled: true,
            modelValue: (__VLS_ctx.permissionExpInfoForm.templateId),
        }], __VLS_functionalComponentArgsRest(__VLS_55), false));
    __VLS_57.slots.default;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.templateList)); _i < _a.length; _i++) {
        var item = _a[_i][0];
        var __VLS_58 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
            value: (item.id),
        }));
        var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
                value: (item.id),
            }], __VLS_functionalComponentArgsRest(__VLS_59), false));
        __VLS_61.slots.default;
        (item.name);
        var __VLS_61;
    }
    var __VLS_57;
    var __VLS_53;
}
if (__VLS_ctx.templateParamConfigs.length > 0) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_62 = {}.ParamInput;
    /** @type {[typeof __VLS_components.ParamInput, ]} */ ;
    // @ts-ignore
    var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
        ref: "templateParamsRef",
        configs: (__VLS_ctx.templateParamConfigs),
        modelValue: (__VLS_ctx.permissionExpInfoForm.templateParams),
    }));
    var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
            ref: "templateParamsRef",
            configs: (__VLS_ctx.templateParamConfigs),
            modelValue: (__VLS_ctx.permissionExpInfoForm.templateParams),
        }], __VLS_functionalComponentArgsRest(__VLS_63), false));
    /** @type {typeof __VLS_ctx.templateParamsRef} */ ;
    var __VLS_66 = {};
    var __VLS_65;
}
if (!__VLS_ctx.permissionExpInfoForm.templateId) {
    var __VLS_68 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({
        field: "expression",
        label: "JEXL 表达式",
    }));
    var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([{
            field: "expression",
            label: "JEXL 表达式",
        }], __VLS_functionalComponentArgsRest(__VLS_69), false));
    __VLS_71.slots.default;
    var __VLS_72 = {}.MonacoEditor;
    /** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
    // @ts-ignore
    var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({
        modelValue: (__VLS_ctx.permissionExpInfoForm.expression),
        language: "jexl",
        editorOption: ({
            contextmenu: false,
        }),
        height: "280px",
    }));
    var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.permissionExpInfoForm.expression),
            language: "jexl",
            editorOption: ({
                contextmenu: false,
            }),
            height: "280px",
        }], __VLS_functionalComponentArgsRest(__VLS_73), false));
    var __VLS_71;
}
var __VLS_76 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({
    field: "desc",
    label: "限制条件描述",
}));
var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([{
        field: "desc",
        label: "限制条件描述",
    }], __VLS_functionalComponentArgsRest(__VLS_77), false));
__VLS_79.slots.default;
var __VLS_80 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_81 = __VLS_asFunctionalComponent(__VLS_80, new __VLS_80({
    modelValue: (__VLS_ctx.permissionExpInfoForm.desc),
    placeholder: "请输入限制条件描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_82 = __VLS_81.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.permissionExpInfoForm.desc),
        placeholder: "请输入限制条件描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_81), false));
var __VLS_79;
var __VLS_84 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_85 = __VLS_asFunctionalComponent(__VLS_84, new __VLS_84({
    hideLabel: true,
}));
var __VLS_86 = __VLS_85.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_85), false));
__VLS_87.slots.default;
var __VLS_88 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_89 = __VLS_asFunctionalComponent(__VLS_88, new __VLS_88({}));
var __VLS_90 = __VLS_89.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_89), false));
__VLS_91.slots.default;
var __VLS_92 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_93 = __VLS_asFunctionalComponent(__VLS_92, new __VLS_92({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_94 = __VLS_93.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_93), false));
__VLS_95.slots.default;
var __VLS_95;
var __VLS_96 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96(__assign({ 'onClick': {} })));
var __VLS_98 = __VLS_97.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_97), false));
var __VLS_100;
var __VLS_101;
var __VLS_102;
var __VLS_103 = {
    onClick: (__VLS_ctx.handleResetPermissionExpInfoForm)
};
__VLS_99.slots.default;
var __VLS_99;
var __VLS_91;
var __VLS_87;
var __VLS_35;
var __VLS_31;
var __VLS_104 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_105 = __VLS_asFunctionalComponent(__VLS_104, new __VLS_104({
    key: "permission_list",
    title: "关联权限",
}));
var __VLS_106 = __VLS_105.apply(void 0, __spreadArray([{
        key: "permission_list",
        title: "关联权限",
    }], __VLS_functionalComponentArgsRest(__VLS_105), false));
__VLS_107.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_108 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_109 = __VLS_asFunctionalComponent(__VLS_108, new __VLS_108({
    data: (__VLS_ctx.permissions),
    bordered: (false),
    pagination: (false),
}));
var __VLS_110 = __VLS_109.apply(void 0, __spreadArray([{
        data: (__VLS_ctx.permissions),
        bordered: (false),
        pagination: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_109), false));
__VLS_111.slots.default;
{
    var __VLS_thisSlot = __VLS_111.slots.columns;
    var __VLS_112 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_113 = __VLS_asFunctionalComponent(__VLS_112, new __VLS_112({
        title: "被授权主体",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_114 = __VLS_113.apply(void 0, __spreadArray([{
            title: "被授权主体",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_113), false));
    __VLS_115.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_115.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handeToPrincipalDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.principal);
    }
    var __VLS_115;
    var __VLS_116 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_117 = __VLS_asFunctionalComponent(__VLS_116, new __VLS_116({
        title: "主体类型",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_118 = __VLS_117.apply(void 0, __spreadArray([{
            title: "主体类型",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_117), false));
    __VLS_119.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_119.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        (record.principalTypeDisplayName);
    }
    var __VLS_119;
    var __VLS_120 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_121 = __VLS_asFunctionalComponent(__VLS_120, new __VLS_120({
        title: "资源组",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_122 = __VLS_121.apply(void 0, __spreadArray([{
            title: "资源组",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_121), false));
    __VLS_123.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_123.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToResourceGroupDetail(record_2.resourceGroupId);
            } }, { class: "table-column-name" }));
        (record_2.resourceGroupName);
    }
    var __VLS_123;
    var __VLS_124 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_125 = __VLS_asFunctionalComponent(__VLS_124, new __VLS_124({
        title: "资源",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_126 = __VLS_125.apply(void 0, __spreadArray([{
            title: "资源",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_125), false));
    __VLS_127.slots.default;
    {
        var __VLS_thisSlot_4 = __VLS_127.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_4)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToResourceDetail(record_3.resourceId);
            } }, { class: "table-column-name" }));
        (record_3.resourceName);
    }
    var __VLS_127;
    var __VLS_128 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_129 = __VLS_asFunctionalComponent(__VLS_128, new __VLS_128({
        title: "权限名称",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_130 = __VLS_129.apply(void 0, __spreadArray([{
            title: "权限名称",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_129), false));
    __VLS_131.slots.default;
    {
        var __VLS_thisSlot_5 = __VLS_131.slots.cell;
        var record_4 = __VLS_getSlotParams(__VLS_thisSlot_5)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToPermissionDetail(record_4.permissionId);
            } }, { class: "table-column-name" }));
        (record_4.permissionName);
    }
    var __VLS_131;
    var __VLS_132 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_133 = __VLS_asFunctionalComponent(__VLS_132, new __VLS_132({
        title: "权限标识",
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_134 = __VLS_133.apply(void 0, __spreadArray([{
            title: "权限标识",
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_133), false));
    __VLS_135.slots.default;
    {
        var __VLS_thisSlot_6 = __VLS_135.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_6)[0].record;
        (record.permissionCode);
    }
    var __VLS_135;
    var __VLS_136 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136({
        title: "操作",
    }));
    var __VLS_138 = __VLS_137.apply(void 0, __spreadArray([{
            title: "操作",
        }], __VLS_functionalComponentArgsRest(__VLS_137), false));
    __VLS_139.slots.default;
    {
        var __VLS_thisSlot_7 = __VLS_139.slots.cell;
        var record_5 = __VLS_getSlotParams(__VLS_thisSlot_7)[0].record;
        var __VLS_140 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_141 = __VLS_asFunctionalComponent(__VLS_140, new __VLS_140({}));
        var __VLS_142 = __VLS_141.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_141), false));
        __VLS_143.slots.default;
        var __VLS_144 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({
            type: "text",
        }));
        var __VLS_146 = __VLS_145.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_145), false));
        __VLS_147.slots.default;
        {
            var __VLS_thisSlot_8 = __VLS_147.slots.icon;
            var __VLS_148 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148({}));
            var __VLS_150 = __VLS_149.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_149), false));
        }
        var __VLS_147;
        {
            var __VLS_thisSlot_9 = __VLS_143.slots.content;
            var __VLS_152 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_154 = __VLS_153.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_153), false));
            var __VLS_156 = void 0;
            var __VLS_157 = void 0;
            var __VLS_158 = void 0;
            var __VLS_159 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleRemoveAuthorizeCondition(record_5.authorizeId);
                }
            };
            __VLS_155.slots.default;
            {
                var __VLS_thisSlot_10 = __VLS_155.slots.icon;
                var __VLS_160 = {}.IconUndo;
                /** @type {[typeof __VLS_components.IconUndo, typeof __VLS_components.iconUndo, ]} */ ;
                // @ts-ignore
                var __VLS_161 = __VLS_asFunctionalComponent(__VLS_160, new __VLS_160({}));
                var __VLS_162 = __VLS_161.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_161), false));
            }
            var __VLS_155;
        }
        var __VLS_143;
    }
    var __VLS_139;
}
var __VLS_111;
var __VLS_107;
var __VLS_23;
var __VLS_3;
var __VLS_164 = {}.ADrawer;
/** @type {[typeof __VLS_components.ADrawer, typeof __VLS_components.aDrawer, typeof __VLS_components.ADrawer, typeof __VLS_components.aDrawer, ]} */ ;
// @ts-ignore
var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164(__assign(__assign({ 'onCancel': {} }, { 'onOk': {} }), { width: (540), visible: (__VLS_ctx.debugDrawerVisible), okText: "调试运行", okLoading: (__VLS_ctx.debugFormSubmitLoading) })));
var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([__assign(__assign({ 'onCancel': {} }, { 'onOk': {} }), { width: (540), visible: (__VLS_ctx.debugDrawerVisible), okText: "调试运行", okLoading: (__VLS_ctx.debugFormSubmitLoading) })], __VLS_functionalComponentArgsRest(__VLS_165), false));
var __VLS_168;
var __VLS_169;
var __VLS_170;
var __VLS_171 = {
    onCancel: (__VLS_ctx.handleCloseDebugDrawer)
};
var __VLS_172 = {
    onOk: (__VLS_ctx.handleDebugFormSubmit)
};
__VLS_167.slots.default;
{
    var __VLS_thisSlot = __VLS_167.slots.title;
}
var __VLS_173 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_174 = __VLS_asFunctionalComponent(__VLS_173, new __VLS_173({
    model: (__VLS_ctx.debugForm),
    rules: (__VLS_ctx.debugFormRules),
    ref: "debugFormRef",
    layout: "vertical",
}));
var __VLS_175 = __VLS_174.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.debugForm),
        rules: (__VLS_ctx.debugFormRules),
        ref: "debugFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_174), false));
/** @type {typeof __VLS_ctx.debugFormRef} */ ;
var __VLS_177 = {};
__VLS_176.slots.default;
var __VLS_179 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_180 = __VLS_asFunctionalComponent(__VLS_179, new __VLS_179({
    field: "context",
    label: "上下文",
}));
var __VLS_181 = __VLS_180.apply(void 0, __spreadArray([{
        field: "context",
        label: "上下文",
    }], __VLS_functionalComponentArgsRest(__VLS_180), false));
__VLS_182.slots.default;
var __VLS_183 = {}.MonacoEditor;
/** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
// @ts-ignore
var __VLS_184 = __VLS_asFunctionalComponent(__VLS_183, new __VLS_183({
    modelValue: (__VLS_ctx.debugForm.context),
    language: "json",
    height: "280px",
    editorOption: ({
        contextmenu: false,
    }),
}));
var __VLS_185 = __VLS_184.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.debugForm.context),
        language: "json",
        height: "280px",
        editorOption: ({
            contextmenu: false,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_184), false));
{
    var __VLS_thisSlot = __VLS_182.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_182;
var __VLS_176;
var __VLS_167;
var __VLS_187 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_188 = __VLS_asFunctionalComponent(__VLS_187, new __VLS_187(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.debugResultModalVisible), footer: (false), maskClosable: (false) })));
var __VLS_189 = __VLS_188.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.debugResultModalVisible), footer: (false), maskClosable: (false) })], __VLS_functionalComponentArgsRest(__VLS_188), false));
var __VLS_191;
var __VLS_192;
var __VLS_193;
var __VLS_194 = {
    onCancel: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.debugResultModalVisible = false;
    }
};
__VLS_190.slots.default;
{
    var __VLS_thisSlot = __VLS_190.slots.title;
}
var __VLS_195 = {}.ADescriptions;
/** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
// @ts-ignore
var __VLS_196 = __VLS_asFunctionalComponent(__VLS_195, new __VLS_195({
    column: (1),
    bordered: true,
}));
var __VLS_197 = __VLS_196.apply(void 0, __spreadArray([{
        column: (1),
        bordered: true,
    }], __VLS_functionalComponentArgsRest(__VLS_196), false));
__VLS_198.slots.default;
var __VLS_199 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_200 = __VLS_asFunctionalComponent(__VLS_199, new __VLS_199({
    label: "状态",
}));
var __VLS_201 = __VLS_200.apply(void 0, __spreadArray([{
        label: "状态",
    }], __VLS_functionalComponentArgsRest(__VLS_200), false));
__VLS_202.slots.default;
(__VLS_ctx.debugResult.success ? "成功" : "失败");
var __VLS_202;
if (__VLS_ctx.debugResult.success) {
    var __VLS_203 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_204 = __VLS_asFunctionalComponent(__VLS_203, new __VLS_203({
        label: "结果",
    }));
    var __VLS_205 = __VLS_204.apply(void 0, __spreadArray([{
            label: "结果",
        }], __VLS_functionalComponentArgsRest(__VLS_204), false));
    __VLS_206.slots.default;
    if (__VLS_ctx.debugResult.execResult) {
        var __VLS_207 = {}.ATag;
        /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
        // @ts-ignore
        var __VLS_208 = __VLS_asFunctionalComponent(__VLS_207, new __VLS_207({
            color: "#00b42a",
        }));
        var __VLS_209 = __VLS_208.apply(void 0, __spreadArray([{
                color: "#00b42a",
            }], __VLS_functionalComponentArgsRest(__VLS_208), false));
        __VLS_210.slots.default;
        {
            var __VLS_thisSlot = __VLS_210.slots.icon;
            var __VLS_211 = {}.IconCheckCircleFill;
            /** @type {[typeof __VLS_components.IconCheckCircleFill, typeof __VLS_components.iconCheckCircleFill, ]} */ ;
            // @ts-ignore
            var __VLS_212 = __VLS_asFunctionalComponent(__VLS_211, new __VLS_211(__assign({ style: {} })));
            var __VLS_213 = __VLS_212.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_212), false));
        }
        var __VLS_210;
    }
    else {
        var __VLS_215 = {}.ATag;
        /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
        // @ts-ignore
        var __VLS_216 = __VLS_asFunctionalComponent(__VLS_215, new __VLS_215({
            color: "#f53f3f",
        }));
        var __VLS_217 = __VLS_216.apply(void 0, __spreadArray([{
                color: "#f53f3f",
            }], __VLS_functionalComponentArgsRest(__VLS_216), false));
        __VLS_218.slots.default;
        {
            var __VLS_thisSlot = __VLS_218.slots.icon;
            var __VLS_219 = {}.IconMinusCircleFill;
            /** @type {[typeof __VLS_components.IconMinusCircleFill, typeof __VLS_components.iconMinusCircleFill, ]} */ ;
            // @ts-ignore
            var __VLS_220 = __VLS_asFunctionalComponent(__VLS_219, new __VLS_219(__assign({ style: {} })));
            var __VLS_221 = __VLS_220.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_220), false));
        }
        var __VLS_218;
    }
    var __VLS_206;
}
else {
    var __VLS_223 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_224 = __VLS_asFunctionalComponent(__VLS_223, new __VLS_223({
        label: "错误",
    }));
    var __VLS_225 = __VLS_224.apply(void 0, __spreadArray([{
            label: "错误",
        }], __VLS_functionalComponentArgsRest(__VLS_224), false));
    __VLS_226.slots.default;
    (__VLS_ctx.debugResult.execResult);
    var __VLS_226;
}
var __VLS_198;
var __VLS_190;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
// @ts-ignore
var __VLS_41 = __VLS_40, __VLS_67 = __VLS_66, __VLS_178 = __VLS_177;
var __VLS_dollars;
var __VLS_self;
