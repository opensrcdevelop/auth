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
import indexTs from "./index";
export default indexTs;
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
(__VLS_ctx.templateName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.templateId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.templateId),
        textColor: "#86909c",
    }], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_12 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_13), false));
var __VLS_16;
var __VLS_17;
var __VLS_18;
var __VLS_19 = {
    onClick: (__VLS_ctx.handleOpenDebugDrawer)
};
__VLS_15.slots.default;
var __VLS_15;
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
    key: "template_info",
    title: "限制条件模板信息",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        key: "template_info",
        title: "限制条件模板信息",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_32 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
    model: (__VLS_ctx.templateInfoForm),
    rules: (__VLS_ctx.templateInfoFormRules),
    ref: "templateInfoFormRef",
    layout: "vertical",
}));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.templateInfoForm),
        rules: (__VLS_ctx.templateInfoFormRules),
        ref: "templateInfoFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_33), false));
/** @type {typeof __VLS_ctx.templateInfoFormRef} */ ;
var __VLS_36 = {};
__VLS_35.slots.default;
var __VLS_38 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
    field: "name",
    label: "模板名称",
}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
        field: "name",
        label: "模板名称",
    }], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    modelValue: (__VLS_ctx.templateInfoForm.name),
    placeholder: "请输入模板名称",
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.templateInfoForm.name),
        placeholder: "请输入模板名称",
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
var __VLS_41;
var __VLS_46 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    field: "desc",
    label: "模板描述",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        field: "desc",
        label: "模板描述",
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    modelValue: (__VLS_ctx.templateInfoForm.desc),
    placeholder: "请输入模板描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.templateInfoForm.desc),
        placeholder: "请输入模板描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
var __VLS_49;
var __VLS_54 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    field: "expression",
    label: "JEXL 表达式",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        field: "expression",
        label: "JEXL 表达式",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
__VLS_57.slots.default;
var __VLS_58 = {}.MonacoEditor;
/** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    modelValue: (__VLS_ctx.templateInfoForm.expression),
    language: "jexl",
    height: "280px",
    editorOption: ({
        contextmenu: false,
        theme: 'jexl-vs-theme',
    }),
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.templateInfoForm.expression),
        language: "jexl",
        height: "280px",
        editorOption: ({
            contextmenu: false,
            theme: 'jexl-vs-theme',
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
var __VLS_57;
var _loop_1 = function (item, index) {
    var __VLS_62 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
        field: ("paramConfigs[".concat(index, "]")),
        label: ("\u6A21\u677F\u53C2\u6570 - ".concat(index + 1)),
        key: (index),
    }));
    var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
            field: ("paramConfigs[".concat(index, "]")),
            label: ("\u6A21\u677F\u53C2\u6570 - ".concat(index + 1)),
            key: (index),
        }], __VLS_functionalComponentArgsRest(__VLS_63), false));
    __VLS_65.slots.default;
    var __VLS_66 = {}.ParamSelect;
    /** @type {[typeof __VLS_components.ParamSelect, ]} */ ;
    // @ts-ignore
    var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66(__assign({ 'onRemove': {} }, { ref: (function (el) {
            if (el)
                __VLS_ctx.paramConfigRefs[index] = el;
        }), modelValue: (__VLS_ctx.templateInfoForm.paramConfigs[index]) })));
    var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([__assign({ 'onRemove': {} }, { ref: (function (el) {
                if (el)
                    __VLS_ctx.paramConfigRefs[index] = el;
            }), modelValue: (__VLS_ctx.templateInfoForm.paramConfigs[index]) })], __VLS_functionalComponentArgsRest(__VLS_67), false));
    var __VLS_70 = void 0;
    var __VLS_71 = void 0;
    var __VLS_72 = void 0;
    var __VLS_73 = {
        onRemove: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleRemoveParamConfig(index);
        }
    };
};
var __VLS_69, __VLS_65;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.templateInfoForm.paramConfigs)); _i < _a.length; _i++) {
    var _b = _a[_i], item = _b[0], index = _b[1];
    _loop_1(item, index);
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "add-param-config" }));
var __VLS_74 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74(__assign({ 'onClick': {} }, { type: "text" })));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_75), false));
var __VLS_78;
var __VLS_79;
var __VLS_80;
var __VLS_81 = {
    onClick: (__VLS_ctx.handleParamConfigModalOpen)
};
__VLS_77.slots.default;
{
    var __VLS_thisSlot = __VLS_77.slots.icon;
    var __VLS_82 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({}));
    var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_83), false));
}
{
    var __VLS_thisSlot = __VLS_77.slots.default;
}
var __VLS_77;
var __VLS_86 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    hideLabel: true,
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_90 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_94 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_95), false));
var __VLS_98;
var __VLS_99;
var __VLS_100;
var __VLS_101 = {
    onClick: (__VLS_ctx.handleTemplateInfoFormSubmit)
};
__VLS_97.slots.default;
var __VLS_97;
var __VLS_102 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102(__assign({ 'onClick': {} })));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_103), false));
var __VLS_106;
var __VLS_107;
var __VLS_108;
var __VLS_109 = {
    onClick: (__VLS_ctx.handleResetTemplateInfoForm)
};
__VLS_105.slots.default;
var __VLS_105;
var __VLS_93;
var __VLS_89;
var __VLS_35;
var __VLS_31;
var __VLS_110 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
    key: "expression_list",
    title: "关联限制条件",
}));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
        key: "expression_list",
        title: "关联限制条件",
    }], __VLS_functionalComponentArgsRest(__VLS_111), false));
__VLS_113.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_114 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    data: (__VLS_ctx.expList),
    bordered: (false),
    scroll: ({ y: '100%' }),
    pagination: (false),
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        data: (__VLS_ctx.expList),
        bordered: (false),
        scroll: ({ y: '100%' }),
        pagination: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
{
    var __VLS_thisSlot = __VLS_117.slots.columns;
    var __VLS_118 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
        title: "条件名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
            title: "条件名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_119), false));
    __VLS_121.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_121.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleToExpDetail(record_1);
            } }, { class: "table-column-name" }));
        (record_1.name);
    }
    var __VLS_121;
    var __VLS_122 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
        title: "描述",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
            title: "描述",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_123), false));
    __VLS_125.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_125.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        (record.desc ? record.desc : "-");
    }
    var __VLS_125;
}
var __VLS_117;
var __VLS_113;
var __VLS_23;
var __VLS_3;
var __VLS_126 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126(__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.addParamConfigModalVisible), maskClosable: (false), hideCancel: (true) })));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.addParamConfigModalVisible), maskClosable: (false), hideCancel: (true) })], __VLS_functionalComponentArgsRest(__VLS_127), false));
var __VLS_130;
var __VLS_131;
var __VLS_132;
var __VLS_133 = {
    onOk: (__VLS_ctx.handleParamConfigModalConfirm)
};
var __VLS_134 = {
    onCancel: (__VLS_ctx.handleParamConfigModalClose)
};
__VLS_129.slots.default;
{
    var __VLS_thisSlot = __VLS_129.slots.title;
}
var __VLS_135 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_136 = __VLS_asFunctionalComponent(__VLS_135, new __VLS_135({
    modelValue: (__VLS_ctx.selectedParamType),
    placeholder: "请选择参数类型",
}));
var __VLS_137 = __VLS_136.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.selectedParamType),
        placeholder: "请选择参数类型",
    }], __VLS_functionalComponentArgsRest(__VLS_136), false));
__VLS_138.slots.default;
for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.parmaTypes)); _c < _d.length; _c++) {
    var _e = _d[_c], item = _e[0], index = _e[1];
    var __VLS_139 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_140 = __VLS_asFunctionalComponent(__VLS_139, new __VLS_139({
        value: (item.value),
    }));
    var __VLS_141 = __VLS_140.apply(void 0, __spreadArray([{
            value: (item.value),
        }], __VLS_functionalComponentArgsRest(__VLS_140), false));
    __VLS_142.slots.default;
    (item.label);
    var __VLS_142;
}
var __VLS_138;
var __VLS_129;
var __VLS_143 = {}.ADrawer;
/** @type {[typeof __VLS_components.ADrawer, typeof __VLS_components.aDrawer, typeof __VLS_components.ADrawer, typeof __VLS_components.aDrawer, ]} */ ;
// @ts-ignore
var __VLS_144 = __VLS_asFunctionalComponent(__VLS_143, new __VLS_143(__assign(__assign({ 'onCancel': {} }, { 'onOk': {} }), { width: (540), visible: (__VLS_ctx.debugDrawerVisible), okText: "调试运行", okLoading: (__VLS_ctx.debugFormSubmitLoading) })));
var __VLS_145 = __VLS_144.apply(void 0, __spreadArray([__assign(__assign({ 'onCancel': {} }, { 'onOk': {} }), { width: (540), visible: (__VLS_ctx.debugDrawerVisible), okText: "调试运行", okLoading: (__VLS_ctx.debugFormSubmitLoading) })], __VLS_functionalComponentArgsRest(__VLS_144), false));
var __VLS_147;
var __VLS_148;
var __VLS_149;
var __VLS_150 = {
    onCancel: (__VLS_ctx.handleCloseDebugDrawer)
};
var __VLS_151 = {
    onOk: (__VLS_ctx.handleDebugFormSubmit)
};
__VLS_146.slots.default;
{
    var __VLS_thisSlot = __VLS_146.slots.title;
}
var __VLS_152 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_153 = __VLS_asFunctionalComponent(__VLS_152, new __VLS_152({
    model: (__VLS_ctx.debugForm),
    rules: (__VLS_ctx.debugFormRules),
    ref: "debugFormRef",
    layout: "vertical",
}));
var __VLS_154 = __VLS_153.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.debugForm),
        rules: (__VLS_ctx.debugFormRules),
        ref: "debugFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_153), false));
/** @type {typeof __VLS_ctx.debugFormRef} */ ;
var __VLS_156 = {};
__VLS_155.slots.default;
var __VLS_158 = {}.ParamInput;
/** @type {[typeof __VLS_components.ParamInput, ]} */ ;
// @ts-ignore
var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158({
    ref: "debugParamRef",
    configs: (__VLS_ctx.debugParamConfigs),
    modelValue: (__VLS_ctx.debugForm.templateParams),
}));
var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([{
        ref: "debugParamRef",
        configs: (__VLS_ctx.debugParamConfigs),
        modelValue: (__VLS_ctx.debugForm.templateParams),
    }], __VLS_functionalComponentArgsRest(__VLS_159), false));
/** @type {typeof __VLS_ctx.debugParamRef} */ ;
var __VLS_162 = {};
var __VLS_161;
var __VLS_164 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_165 = __VLS_asFunctionalComponent(__VLS_164, new __VLS_164({
    field: "context",
    label: "上下文",
}));
var __VLS_166 = __VLS_165.apply(void 0, __spreadArray([{
        field: "context",
        label: "上下文",
    }], __VLS_functionalComponentArgsRest(__VLS_165), false));
__VLS_167.slots.default;
var __VLS_168 = {}.MonacoEditor;
/** @type {[typeof __VLS_components.MonacoEditor, typeof __VLS_components.monacoEditor, ]} */ ;
// @ts-ignore
var __VLS_169 = __VLS_asFunctionalComponent(__VLS_168, new __VLS_168({
    modelValue: (__VLS_ctx.debugForm.context),
    language: "json",
    height: "280px",
    editorOption: ({
        contextmenu: false,
    }),
}));
var __VLS_170 = __VLS_169.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.debugForm.context),
        language: "json",
        height: "280px",
        editorOption: ({
            contextmenu: false,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_169), false));
{
    var __VLS_thisSlot = __VLS_167.slots.extra;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
}
var __VLS_167;
var __VLS_155;
var __VLS_146;
var __VLS_172 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_173 = __VLS_asFunctionalComponent(__VLS_172, new __VLS_172(__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.debugResultModalVisible), footer: (false), maskClosable: (false) })));
var __VLS_174 = __VLS_173.apply(void 0, __spreadArray([__assign({ 'onCancel': {} }, { visible: (__VLS_ctx.debugResultModalVisible), footer: (false), maskClosable: (false) })], __VLS_functionalComponentArgsRest(__VLS_173), false));
var __VLS_176;
var __VLS_177;
var __VLS_178;
var __VLS_179 = {
    onCancel: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.debugResultModalVisible = false;
    }
};
__VLS_175.slots.default;
{
    var __VLS_thisSlot = __VLS_175.slots.title;
}
var __VLS_180 = {}.ADescriptions;
/** @type {[typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, typeof __VLS_components.ADescriptions, typeof __VLS_components.aDescriptions, ]} */ ;
// @ts-ignore
var __VLS_181 = __VLS_asFunctionalComponent(__VLS_180, new __VLS_180({
    column: (1),
    bordered: true,
}));
var __VLS_182 = __VLS_181.apply(void 0, __spreadArray([{
        column: (1),
        bordered: true,
    }], __VLS_functionalComponentArgsRest(__VLS_181), false));
__VLS_183.slots.default;
var __VLS_184 = {}.ADescriptionsItem;
/** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
// @ts-ignore
var __VLS_185 = __VLS_asFunctionalComponent(__VLS_184, new __VLS_184({
    label: "状态",
}));
var __VLS_186 = __VLS_185.apply(void 0, __spreadArray([{
        label: "状态",
    }], __VLS_functionalComponentArgsRest(__VLS_185), false));
__VLS_187.slots.default;
(__VLS_ctx.debugResult.success ? "成功" : "失败");
var __VLS_187;
if (__VLS_ctx.debugResult.success) {
    var __VLS_188 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_189 = __VLS_asFunctionalComponent(__VLS_188, new __VLS_188({
        label: "结果",
    }));
    var __VLS_190 = __VLS_189.apply(void 0, __spreadArray([{
            label: "结果",
        }], __VLS_functionalComponentArgsRest(__VLS_189), false));
    __VLS_191.slots.default;
    if (__VLS_ctx.debugResult.execResult) {
        var __VLS_192 = {}.ATag;
        /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
        // @ts-ignore
        var __VLS_193 = __VLS_asFunctionalComponent(__VLS_192, new __VLS_192({
            color: "#00b42a",
        }));
        var __VLS_194 = __VLS_193.apply(void 0, __spreadArray([{
                color: "#00b42a",
            }], __VLS_functionalComponentArgsRest(__VLS_193), false));
        __VLS_195.slots.default;
        {
            var __VLS_thisSlot = __VLS_195.slots.icon;
            var __VLS_196 = {}.IconCheckCircleFill;
            /** @type {[typeof __VLS_components.IconCheckCircleFill, typeof __VLS_components.iconCheckCircleFill, ]} */ ;
            // @ts-ignore
            var __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196(__assign({ style: {} })));
            var __VLS_198 = __VLS_197.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_197), false));
        }
        var __VLS_195;
    }
    else {
        var __VLS_200 = {}.ATag;
        /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
        // @ts-ignore
        var __VLS_201 = __VLS_asFunctionalComponent(__VLS_200, new __VLS_200({
            color: "#f53f3f",
        }));
        var __VLS_202 = __VLS_201.apply(void 0, __spreadArray([{
                color: "#f53f3f",
            }], __VLS_functionalComponentArgsRest(__VLS_201), false));
        __VLS_203.slots.default;
        {
            var __VLS_thisSlot = __VLS_203.slots.icon;
            var __VLS_204 = {}.IconMinusCircleFill;
            /** @type {[typeof __VLS_components.IconMinusCircleFill, typeof __VLS_components.iconMinusCircleFill, ]} */ ;
            // @ts-ignore
            var __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204(__assign({ style: {} })));
            var __VLS_206 = __VLS_205.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_205), false));
        }
        var __VLS_203;
    }
    var __VLS_191;
}
else {
    var __VLS_208 = {}.ADescriptionsItem;
    /** @type {[typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, typeof __VLS_components.ADescriptionsItem, typeof __VLS_components.aDescriptionsItem, ]} */ ;
    // @ts-ignore
    var __VLS_209 = __VLS_asFunctionalComponent(__VLS_208, new __VLS_208({
        label: "错误",
    }));
    var __VLS_210 = __VLS_209.apply(void 0, __spreadArray([{
            label: "错误",
        }], __VLS_functionalComponentArgsRest(__VLS_209), false));
    __VLS_211.slots.default;
    (__VLS_ctx.debugResult.execResult);
    var __VLS_211;
}
var __VLS_183;
var __VLS_175;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['add-param-config']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
// @ts-ignore
var __VLS_37 = __VLS_36, __VLS_157 = __VLS_156, __VLS_163 = __VLS_162;
var __VLS_dollars;
var __VLS_self;
