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
(__VLS_ctx.dataSourceName);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "id" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
var __VLS_8 = {}.CopyText;
/** @type {[typeof __VLS_components.CopyText, typeof __VLS_components.copyText, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
    text: (__VLS_ctx.dataSourceId),
    textColor: "#86909c",
}));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
        text: (__VLS_ctx.dataSourceId),
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
    onClick: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.hanleTestConn();
    }
};
__VLS_15.slots.default;
var __VLS_15;
var __VLS_3;
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
    key: "data_source_info",
    title: "数据源信息",
}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
        key: "data_source_info",
        title: "数据源信息",
    }], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_31.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info-title" }));
var __VLS_32 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.dataSourceInfoForm), layout: "vertical", ref: "dataSourceInfoFormRef", rules: (__VLS_ctx.dataSourceInfoFormRules) })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign({ 'onSubmitSuccess': {} }, { model: (__VLS_ctx.dataSourceInfoForm), layout: "vertical", ref: "dataSourceInfoFormRef", rules: (__VLS_ctx.dataSourceInfoFormRules) })], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_36;
var __VLS_37;
var __VLS_38;
var __VLS_39 = {
    onSubmitSuccess: (__VLS_ctx.handleDataSourceInfoFormSubmit)
};
/** @type {typeof __VLS_ctx.dataSourceInfoFormRef} */ ;
var __VLS_40 = {};
__VLS_35.slots.default;
var __VLS_42 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    gutter: (24),
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    span: (12),
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    field: "name",
    label: "数据源名称",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        field: "name",
        label: "数据源名称",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
__VLS_53.slots.default;
var __VLS_54 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
    modelValue: (__VLS_ctx.dataSourceInfoForm.name),
    placeholder: "请输入数据源名称",
}));
var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.name),
        placeholder: "请输入数据源名称",
    }], __VLS_functionalComponentArgsRest(__VLS_55), false));
var __VLS_53;
var __VLS_49;
var __VLS_58 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
    span: (12),
}));
var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_59), false));
__VLS_61.slots.default;
var __VLS_62 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
    field: "type",
    label: "数据源类型",
}));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
        field: "type",
        label: "数据源类型",
    }], __VLS_functionalComponentArgsRest(__VLS_63), false));
__VLS_65.slots.default;
var __VLS_66 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
    modelValue: (__VLS_ctx.dataSourceInfoForm.type),
    disabled: true,
}));
var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.type),
        disabled: true,
    }], __VLS_functionalComponentArgsRest(__VLS_67), false));
__VLS_69.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.dataSourceTypeList)); _i < _a.length; _i++) {
    var item = _a[_i][0];
    var __VLS_70 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
        value: (item.value),
        label: (item.label),
    }));
    var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
            value: (item.value),
            label: (item.label),
        }], __VLS_functionalComponentArgsRest(__VLS_71), false));
}
var __VLS_69;
var __VLS_65;
var __VLS_61;
var __VLS_74 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
    span: (12),
}));
var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_75), false));
__VLS_77.slots.default;
var __VLS_78 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
    field: "database",
    label: "数据库",
}));
var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
        field: "database",
        label: "数据库",
    }], __VLS_functionalComponentArgsRest(__VLS_79), false));
__VLS_81.slots.default;
var __VLS_82 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
    disabled: true,
    modelValue: (__VLS_ctx.dataSourceInfoForm.database),
}));
var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
        disabled: true,
        modelValue: (__VLS_ctx.dataSourceInfoForm.database),
    }], __VLS_functionalComponentArgsRest(__VLS_83), false));
var __VLS_81;
var __VLS_77;
var __VLS_86 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
    span: (12),
}));
var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_87), false));
__VLS_89.slots.default;
var __VLS_90 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
    field: "schema",
    label: "模式",
}));
var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
        field: "schema",
        label: "模式",
    }], __VLS_functionalComponentArgsRest(__VLS_91), false));
__VLS_93.slots.default;
var __VLS_94 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    disabled: true,
    modelValue: (__VLS_ctx.dataSourceInfoForm.schema),
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        disabled: true,
        modelValue: (__VLS_ctx.dataSourceInfoForm.schema),
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
var __VLS_93;
var __VLS_89;
var __VLS_98 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
    span: (12),
}));
var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_99), false));
__VLS_101.slots.default;
var __VLS_102 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    field: "host",
    label: "主机地址",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        field: "host",
        label: "主机地址",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_106 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
    modelValue: (__VLS_ctx.dataSourceInfoForm.host),
    placeholder: "请输入主机地址",
}));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.host),
        placeholder: "请输入主机地址",
    }], __VLS_functionalComponentArgsRest(__VLS_107), false));
var __VLS_105;
var __VLS_101;
var __VLS_110 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
    span: (12),
}));
var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_111), false));
__VLS_113.slots.default;
var __VLS_114 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
    field: "port",
    label: "端口号",
}));
var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
        field: "port",
        label: "端口号",
    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
__VLS_117.slots.default;
var __VLS_118 = {}.AInputNumber;
/** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
    modelValue: (__VLS_ctx.dataSourceInfoForm.port),
    min: (0),
    hideButton: true,
    placeholder: "请输入端口号",
}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.port),
        min: (0),
        hideButton: true,
        placeholder: "请输入端口号",
    }], __VLS_functionalComponentArgsRest(__VLS_119), false));
var __VLS_117;
var __VLS_113;
var __VLS_122 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122({
    span: (12),
}));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_123), false));
__VLS_125.slots.default;
var __VLS_126 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_127 = __VLS_asFunctionalComponent(__VLS_126, new __VLS_126({
    field: "username",
    label: "用户名",
}));
var __VLS_128 = __VLS_127.apply(void 0, __spreadArray([{
        field: "username",
        label: "用户名",
    }], __VLS_functionalComponentArgsRest(__VLS_127), false));
__VLS_129.slots.default;
var __VLS_130 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
    modelValue: (__VLS_ctx.dataSourceInfoForm.username),
    placeholder: "请输入用户名",
}));
var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.username),
        placeholder: "请输入用户名",
    }], __VLS_functionalComponentArgsRest(__VLS_131), false));
var __VLS_129;
var __VLS_125;
var __VLS_134 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_135 = __VLS_asFunctionalComponent(__VLS_134, new __VLS_134({
    span: (12),
}));
var __VLS_136 = __VLS_135.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_135), false));
__VLS_137.slots.default;
var __VLS_138 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_139 = __VLS_asFunctionalComponent(__VLS_138, new __VLS_138({
    field: "password",
    label: "密码",
}));
var __VLS_140 = __VLS_139.apply(void 0, __spreadArray([{
        field: "password",
        label: "密码",
    }], __VLS_functionalComponentArgsRest(__VLS_139), false));
__VLS_141.slots.default;
var __VLS_142 = {}.AInputPassword;
/** @type {[typeof __VLS_components.AInputPassword, typeof __VLS_components.aInputPassword, ]} */ ;
// @ts-ignore
var __VLS_143 = __VLS_asFunctionalComponent(__VLS_142, new __VLS_142({
    modelValue: (__VLS_ctx.dataSourceInfoForm.password),
    placeholder: "请输入密码",
}));
var __VLS_144 = __VLS_143.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.password),
        placeholder: "请输入密码",
    }], __VLS_functionalComponentArgsRest(__VLS_143), false));
var __VLS_141;
var __VLS_137;
var __VLS_45;
var __VLS_146 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_147 = __VLS_asFunctionalComponent(__VLS_146, new __VLS_146({
    field: "jdbcParams",
    label: "JDBC 参数",
}));
var __VLS_148 = __VLS_147.apply(void 0, __spreadArray([{
        field: "jdbcParams",
        label: "JDBC 参数",
    }], __VLS_functionalComponentArgsRest(__VLS_147), false));
__VLS_149.slots.default;
var __VLS_150 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_151 = __VLS_asFunctionalComponent(__VLS_150, new __VLS_150({
    modelValue: (__VLS_ctx.dataSourceInfoForm.jdbcParams),
    placeholder: "请输入 JDBC 参数",
}));
var __VLS_152 = __VLS_151.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.jdbcParams),
        placeholder: "请输入 JDBC 参数",
    }], __VLS_functionalComponentArgsRest(__VLS_151), false));
var __VLS_149;
var __VLS_154 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_155 = __VLS_asFunctionalComponent(__VLS_154, new __VLS_154({
    field: "desc",
    label: "数据源描述",
}));
var __VLS_156 = __VLS_155.apply(void 0, __spreadArray([{
        field: "desc",
        label: "数据源描述",
    }], __VLS_functionalComponentArgsRest(__VLS_155), false));
__VLS_157.slots.default;
var __VLS_158 = {}.ATextarea;
/** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
// @ts-ignore
var __VLS_159 = __VLS_asFunctionalComponent(__VLS_158, new __VLS_158({
    modelValue: (__VLS_ctx.dataSourceInfoForm.desc),
    placeholder: "请输入数据源描述",
    autoSize: ({
        minRows: 3,
        maxRows: 5,
    }),
}));
var __VLS_160 = __VLS_159.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.dataSourceInfoForm.desc),
        placeholder: "请输入数据源描述",
        autoSize: ({
            minRows: 3,
            maxRows: 5,
        }),
    }], __VLS_functionalComponentArgsRest(__VLS_159), false));
var __VLS_157;
var __VLS_162 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_163 = __VLS_asFunctionalComponent(__VLS_162, new __VLS_162({
    hideLabel: true,
}));
var __VLS_164 = __VLS_163.apply(void 0, __spreadArray([{
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_163), false));
__VLS_165.slots.default;
var __VLS_166 = {}.ASpace;
/** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
// @ts-ignore
var __VLS_167 = __VLS_asFunctionalComponent(__VLS_166, new __VLS_166({}));
var __VLS_168 = __VLS_167.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_167), false));
__VLS_169.slots.default;
var __VLS_170 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_171 = __VLS_asFunctionalComponent(__VLS_170, new __VLS_170({
    type: "primary",
    htmlType: "submit",
}));
var __VLS_172 = __VLS_171.apply(void 0, __spreadArray([{
        type: "primary",
        htmlType: "submit",
    }], __VLS_functionalComponentArgsRest(__VLS_171), false));
__VLS_173.slots.default;
var __VLS_173;
var __VLS_174 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_175 = __VLS_asFunctionalComponent(__VLS_174, new __VLS_174(__assign({ 'onClick': {} })));
var __VLS_176 = __VLS_175.apply(void 0, __spreadArray([__assign({ 'onClick': {} })], __VLS_functionalComponentArgsRest(__VLS_175), false));
var __VLS_178;
var __VLS_179;
var __VLS_180;
var __VLS_181 = {
    onClick: (__VLS_ctx.handleResetDataSourceInfoForm)
};
__VLS_177.slots.default;
var __VLS_177;
var __VLS_169;
var __VLS_165;
var __VLS_35;
var __VLS_31;
var __VLS_182 = {}.ATabPane;
/** @type {[typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, typeof __VLS_components.ATabPane, typeof __VLS_components.aTabPane, ]} */ ;
// @ts-ignore
var __VLS_183 = __VLS_asFunctionalComponent(__VLS_182, new __VLS_182({
    key: "table_list",
    title: "表列表",
}));
var __VLS_184 = __VLS_183.apply(void 0, __spreadArray([{
        key: "table_list",
        title: "表列表",
    }], __VLS_functionalComponentArgsRest(__VLS_183), false));
__VLS_185.slots.default;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "tab-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "table-list-header" }));
var __VLS_186 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_187 = __VLS_asFunctionalComponent(__VLS_186, new __VLS_186(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入表名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.tableSearchKeyword) })));
var __VLS_188 = __VLS_187.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '320px' }) }), { placeholder: "输入表名称进行搜索", allowClear: true, modelValue: (__VLS_ctx.tableSearchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_187), false));
var __VLS_190;
var __VLS_191;
var __VLS_192;
var __VLS_193 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTableList(__VLS_ctx.dataSourceId, 1, 15);
    }
};
var __VLS_194 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTableList(__VLS_ctx.dataSourceId, 1, 15);
    }
};
var __VLS_195 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetTableList(__VLS_ctx.dataSourceId, 1, 15);
    }
};
var __VLS_189;
var __VLS_196 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_197 = __VLS_asFunctionalComponent(__VLS_196, new __VLS_196(__assign({ 'onClick': {} }, { type: "primary", disabled: (__VLS_ctx.saveBtnDisabled) })));
var __VLS_198 = __VLS_197.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary", disabled: (__VLS_ctx.saveBtnDisabled) })], __VLS_functionalComponentArgsRest(__VLS_197), false));
var __VLS_200;
var __VLS_201;
var __VLS_202;
var __VLS_203 = {
    onClick: (__VLS_ctx.handleSaveTableList)
};
__VLS_199.slots.default;
var __VLS_199;
var __VLS_204 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_205 = __VLS_asFunctionalComponent(__VLS_204, new __VLS_204(__assign(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { class: "table-list" }), { data: (__VLS_ctx.tableList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.tableListPagination.pagination) })));
var __VLS_206 = __VLS_205.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { class: "table-list" }), { data: (__VLS_ctx.tableList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.tableListPagination.pagination) })], __VLS_functionalComponentArgsRest(__VLS_205), false));
var __VLS_208;
var __VLS_209;
var __VLS_210;
var __VLS_211 = {
    onPageChange: (__VLS_ctx.handleTableListPageChange)
};
var __VLS_212 = {
    onPageSizeChange: (__VLS_ctx.handleTableListPageSizeChange)
};
__VLS_207.slots.default;
{
    var __VLS_thisSlot = __VLS_207.slots.columns;
    var __VLS_213 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_214 = __VLS_asFunctionalComponent(__VLS_213, new __VLS_213({
        title: "编辑状态",
        width: (100),
    }));
    var __VLS_215 = __VLS_214.apply(void 0, __spreadArray([{
            title: "编辑状态",
            width: (100),
        }], __VLS_functionalComponentArgsRest(__VLS_214), false));
    __VLS_216.slots.default;
    {
        var __VLS_thisSlot_1 = __VLS_216.slots.cell;
        var record_1 = __VLS_getSlotParams(__VLS_thisSlot_1)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "edit-status-container" }));
        var __VLS_217 = {}.transition;
        /** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
        // @ts-ignore
        var __VLS_218 = __VLS_asFunctionalComponent(__VLS_217, new __VLS_217({
            name: "fade",
        }));
        var __VLS_219 = __VLS_218.apply(void 0, __spreadArray([{
                name: "fade",
            }], __VLS_functionalComponentArgsRest(__VLS_218), false));
        __VLS_220.slots.default;
        if (!record_1._isHovering || !__VLS_ctx.isRowModified(record_1)) {
            var __VLS_221 = {}.ATag;
            /** @type {[typeof __VLS_components.ATag, typeof __VLS_components.aTag, typeof __VLS_components.ATag, typeof __VLS_components.aTag, ]} */ ;
            // @ts-ignore
            var __VLS_222 = __VLS_asFunctionalComponent(__VLS_221, new __VLS_221(__assign(__assign({ 'onMouseenter': {} }, { class: "status-tag" }), { class: ({ modified: __VLS_ctx.isRowModified(record_1) }) })));
            var __VLS_223 = __VLS_222.apply(void 0, __spreadArray([__assign(__assign({ 'onMouseenter': {} }, { class: "status-tag" }), { class: ({ modified: __VLS_ctx.isRowModified(record_1) }) })], __VLS_functionalComponentArgsRest(__VLS_222), false));
            var __VLS_225 = void 0;
            var __VLS_226 = void 0;
            var __VLS_227 = void 0;
            var __VLS_228 = {
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
            __VLS_224.slots.default;
            (__VLS_ctx.isRowModified(record_1) ? "已编辑" : "未编辑");
            var __VLS_224;
        }
        var __VLS_220;
        var __VLS_229 = {}.transition;
        /** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
        // @ts-ignore
        var __VLS_230 = __VLS_asFunctionalComponent(__VLS_229, new __VLS_229({
            name: "fade",
        }));
        var __VLS_231 = __VLS_230.apply(void 0, __spreadArray([{
                name: "fade",
            }], __VLS_functionalComponentArgsRest(__VLS_230), false));
        __VLS_232.slots.default;
        if (record_1._isHovering && __VLS_ctx.isRowModified(record_1)) {
            var __VLS_233 = {}.AButton;
            /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
            // @ts-ignore
            var __VLS_234 = __VLS_asFunctionalComponent(__VLS_233, new __VLS_233(__assign(__assign(__assign({ 'onMouseleave': {} }, { 'onClick': {} }), { class: "status-btn" }), { type: "outline", size: "mini", status: "warning" })));
            var __VLS_235 = __VLS_234.apply(void 0, __spreadArray([__assign(__assign(__assign({ 'onMouseleave': {} }, { 'onClick': {} }), { class: "status-btn" }), { type: "outline", size: "mini", status: "warning" })], __VLS_functionalComponentArgsRest(__VLS_234), false));
            var __VLS_237 = void 0;
            var __VLS_238 = void 0;
            var __VLS_239 = void 0;
            var __VLS_240 = {
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
            var __VLS_241 = {
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
            __VLS_236.slots.default;
            var __VLS_236;
        }
        var __VLS_232;
    }
    var __VLS_216;
    var __VLS_242 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_243 = __VLS_asFunctionalComponent(__VLS_242, new __VLS_242({
        title: "表名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_244 = __VLS_243.apply(void 0, __spreadArray([{
            title: "表名称",
            ellipsis: true,
            tooltip: true,
            sortable: ({
                sortDirections: ['ascend', 'descend'],
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_243), false));
    __VLS_245.slots.default;
    {
        var __VLS_thisSlot_2 = __VLS_245.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_2)[0].record;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleOpenTableFieldListDrawer(record_2);
            } }, { class: "table-column-name" }));
        (record_2.name);
    }
    var __VLS_245;
    var __VLS_246 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_247 = __VLS_asFunctionalComponent(__VLS_246, new __VLS_246({
        title: "表注释",
    }));
    var __VLS_248 = __VLS_247.apply(void 0, __spreadArray([{
            title: "表注释",
        }], __VLS_functionalComponentArgsRest(__VLS_247), false));
    __VLS_249.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_249.slots.cell;
        var record_3 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_250 = {}.ASpace;
        /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
        // @ts-ignore
        var __VLS_251 = __VLS_asFunctionalComponent(__VLS_250, new __VLS_250({}));
        var __VLS_252 = __VLS_251.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_251), false));
        __VLS_253.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "ellipsis-text" }));
        (record_3.remark ? record_3.remark : "-");
        var __VLS_254 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_255 = __VLS_asFunctionalComponent(__VLS_254, new __VLS_254(__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })));
        var __VLS_256 = __VLS_255.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_255), false));
        var __VLS_258 = void 0;
        var __VLS_259 = void 0;
        var __VLS_260 = void 0;
        var __VLS_261 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleOpenTextEditorModal(record_3);
            }
        };
        __VLS_257.slots.default;
        {
            var __VLS_thisSlot_4 = __VLS_257.slots.icon;
            var __VLS_262 = {}.IconEdit;
            /** @type {[typeof __VLS_components.IconEdit, typeof __VLS_components.iconEdit, ]} */ ;
            // @ts-ignore
            var __VLS_263 = __VLS_asFunctionalComponent(__VLS_262, new __VLS_262({}));
            var __VLS_264 = __VLS_263.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_263), false));
        }
        var __VLS_257;
        var __VLS_253;
    }
    var __VLS_249;
    var __VLS_266 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_267 = __VLS_asFunctionalComponent(__VLS_266, new __VLS_266({
        title: "补充信息",
    }));
    var __VLS_268 = __VLS_267.apply(void 0, __spreadArray([{
            title: "补充信息",
        }], __VLS_functionalComponentArgsRest(__VLS_267), false));
    __VLS_269.slots.default;
    {
        var __VLS_thisSlot_5 = __VLS_269.slots.cell;
        var record_4 = __VLS_getSlotParams(__VLS_thisSlot_5)[0].record;
        var __VLS_270 = {}.ASpace;
        /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
        // @ts-ignore
        var __VLS_271 = __VLS_asFunctionalComponent(__VLS_270, new __VLS_270({}));
        var __VLS_272 = __VLS_271.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_271), false));
        __VLS_273.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "ellipsis-text" }));
        (record_4.additionalInfo ? record_4.additionalInfo : "-");
        var __VLS_274 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_275 = __VLS_asFunctionalComponent(__VLS_274, new __VLS_274(__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })));
        var __VLS_276 = __VLS_275.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { shape: "circle", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_275), false));
        var __VLS_278 = void 0;
        var __VLS_279 = void 0;
        var __VLS_280 = void 0;
        var __VLS_281 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleOpenMdEditorModal(record_4);
            }
        };
        __VLS_277.slots.default;
        {
            var __VLS_thisSlot_6 = __VLS_277.slots.icon;
            var __VLS_282 = {}.IconEdit;
            /** @type {[typeof __VLS_components.IconEdit, typeof __VLS_components.iconEdit, ]} */ ;
            // @ts-ignore
            var __VLS_283 = __VLS_asFunctionalComponent(__VLS_282, new __VLS_282({}));
            var __VLS_284 = __VLS_283.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_283), false));
        }
        var __VLS_277;
        var __VLS_273;
    }
    var __VLS_269;
    var __VLS_286 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_287 = __VLS_asFunctionalComponent(__VLS_286, new __VLS_286({
        title: "是否使用",
        width: (100),
    }));
    var __VLS_288 = __VLS_287.apply(void 0, __spreadArray([{
            title: "是否使用",
            width: (100),
        }], __VLS_functionalComponentArgsRest(__VLS_287), false));
    __VLS_289.slots.default;
    {
        var __VLS_thisSlot_7 = __VLS_289.slots.cell;
        var record = __VLS_getSlotParams(__VLS_thisSlot_7)[0].record;
        var __VLS_290 = {}.ASwitch;
        /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
        // @ts-ignore
        var __VLS_291 = __VLS_asFunctionalComponent(__VLS_290, new __VLS_290({
            type: "round",
            size: "small",
            modelValue: (record.toUse),
        }));
        var __VLS_292 = __VLS_291.apply(void 0, __spreadArray([{
                type: "round",
                size: "small",
                modelValue: (record.toUse),
            }], __VLS_functionalComponentArgsRest(__VLS_291), false));
    }
    var __VLS_289;
}
var __VLS_207;
var __VLS_185;
var __VLS_23;
var __VLS_294 = {}.TextEditorModal;
/** @type {[typeof __VLS_components.TextEditorModal, ]} */ ;
// @ts-ignore
var __VLS_295 = __VLS_asFunctionalComponent(__VLS_294, new __VLS_294(__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.textEditorModalVisible), content: (__VLS_ctx.textEditorModalContent), title: (__VLS_ctx.textEditorModalTitle) })));
var __VLS_296 = __VLS_295.apply(void 0, __spreadArray([__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.textEditorModalVisible), content: (__VLS_ctx.textEditorModalContent), title: (__VLS_ctx.textEditorModalTitle) })], __VLS_functionalComponentArgsRest(__VLS_295), false));
var __VLS_298;
var __VLS_299;
var __VLS_300;
var __VLS_301 = {
    onClose: (__VLS_ctx.handleCloseTextEditorModal)
};
var __VLS_302 = {
    onConfirm: (__VLS_ctx.handleTextEditorModalConfirm)
};
var __VLS_297;
var __VLS_303 = {}.MdEditorModal;
/** @type {[typeof __VLS_components.MdEditorModal, ]} */ ;
// @ts-ignore
var __VLS_304 = __VLS_asFunctionalComponent(__VLS_303, new __VLS_303(__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.mdEditorModalVisible), content: (__VLS_ctx.mdEditorModalContent), title: (__VLS_ctx.mdEditorModalTitle) })));
var __VLS_305 = __VLS_304.apply(void 0, __spreadArray([__assign(__assign({ 'onClose': {} }, { 'onConfirm': {} }), { visible: (__VLS_ctx.mdEditorModalVisible), content: (__VLS_ctx.mdEditorModalContent), title: (__VLS_ctx.mdEditorModalTitle) })], __VLS_functionalComponentArgsRest(__VLS_304), false));
var __VLS_307;
var __VLS_308;
var __VLS_309;
var __VLS_310 = {
    onClose: (__VLS_ctx.handleCloseMdEditorModal)
};
var __VLS_311 = {
    onConfirm: (__VLS_ctx.handleMdEditorModalConfirm)
};
var __VLS_306;
var __VLS_312 = {}.TableFieldListDrawer;
/** @type {[typeof __VLS_components.TableFieldListDrawer, ]} */ ;
// @ts-ignore
var __VLS_313 = __VLS_asFunctionalComponent(__VLS_312, new __VLS_312(__assign({ 'onClose': {} }, { visible: (__VLS_ctx.tableFieldListDrawerVisible), tableId: (__VLS_ctx.tableFieldListDrawerTableId), title: (__VLS_ctx.tableFieldListDrawerTitle) })));
var __VLS_314 = __VLS_313.apply(void 0, __spreadArray([__assign({ 'onClose': {} }, { visible: (__VLS_ctx.tableFieldListDrawerVisible), tableId: (__VLS_ctx.tableFieldListDrawerTableId), title: (__VLS_ctx.tableFieldListDrawerTitle) })], __VLS_functionalComponentArgsRest(__VLS_313), false));
var __VLS_316;
var __VLS_317;
var __VLS_318;
var __VLS_319 = {
    onClose: (__VLS_ctx.handleCloseTableFieldListDrawer)
};
var __VLS_315;
/** @type {__VLS_StyleScopedClasses['detail-header']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['id']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['info-title']} */ ;
/** @type {__VLS_StyleScopedClasses['tab-container']} */ ;
/** @type {__VLS_StyleScopedClasses['table-list-header']} */ ;
/** @type {__VLS_StyleScopedClasses['table-list']} */ ;
/** @type {__VLS_StyleScopedClasses['edit-status-container']} */ ;
/** @type {__VLS_StyleScopedClasses['status-tag']} */ ;
/** @type {__VLS_StyleScopedClasses['modified']} */ ;
/** @type {__VLS_StyleScopedClasses['status-btn']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-name']} */ ;
/** @type {__VLS_StyleScopedClasses['ellipsis-text']} */ ;
/** @type {__VLS_StyleScopedClasses['ellipsis-text']} */ ;
// @ts-ignore
var __VLS_41 = __VLS_40;
var __VLS_dollars;
var __VLS_self;
