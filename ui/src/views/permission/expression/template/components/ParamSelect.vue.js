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
import { ref } from "vue";
var props = defineProps();
var emits = defineEmits();
var paramConfigRef = ref();
var paramConfigRules = {
    name: [{ required: true, message: "参数名称未填写" }],
    code: [
        { required: true, message: "参数标识未填写" },
        {
            validator: function (value, cb) {
                if (!/^[A-Za-z0-9\_]+$/.test(value.code)) {
                    cb("参数标识只允许包含英文字母、数字、下划线_");
                }
                else {
                    cb();
                }
            },
        },
    ],
    required: [{ required: true, message: "是否必填未选择" }],
    multiple: [{ required: true, message: "是否多选未选择" }],
};
var handleAddOption = function () {
    emits("update:modelValue", __assign(__assign({}, props.modelValue), { options: __spreadArray(__spreadArray([], props.modelValue.options, true), [""], false) }));
};
var handleRemoveOption = function (index) {
    emits("update:modelValue", __assign(__assign({}, props.modelValue), { options: props.modelValue.options.filter(function (_, i) { return i !== index; }) }));
};
var handleRemoveParamConfig = function () {
    emits("remove");
};
var validate = function () {
    return paramConfigRef.value.validate();
};
var reset = function () {
    paramConfigRef.value.resetFields();
};
var __VLS_exposed = {
    validate: validate,
    reset: reset,
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "param-container" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
var __VLS_0 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    model: (__VLS_ctx.modelValue),
    rules: (__VLS_ctx.paramConfigRules),
    ref: "paramConfigRef",
    layout: "vertical",
}));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.modelValue),
        rules: (__VLS_ctx.paramConfigRules),
        ref: "paramConfigRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_1), false));
/** @type {typeof __VLS_ctx.paramConfigRef} */ ;
var __VLS_4 = {};
__VLS_3.slots.default;
var __VLS_6 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_7 = __VLS_asFunctionalComponent(__VLS_6, new __VLS_6({
    gutter: (24),
}));
var __VLS_8 = __VLS_7.apply(void 0, __spreadArray([{
        gutter: (24),
    }], __VLS_functionalComponentArgsRest(__VLS_7), false));
__VLS_9.slots.default;
var __VLS_10 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_11 = __VLS_asFunctionalComponent(__VLS_10, new __VLS_10({
    span: (8),
}));
var __VLS_12 = __VLS_11.apply(void 0, __spreadArray([{
        span: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_11), false));
__VLS_13.slots.default;
var __VLS_14 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
    field: "name",
    label: "参数名称",
}));
var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
        field: "name",
        label: "参数名称",
    }], __VLS_functionalComponentArgsRest(__VLS_15), false));
__VLS_17.slots.default;
var __VLS_18 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
    modelValue: (__VLS_ctx.modelValue.name),
    placeholder: "请输入参数名称",
    error: (false),
}));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelValue.name),
        placeholder: "请输入参数名称",
        error: (false),
    }], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_17;
var __VLS_13;
var __VLS_22 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
    span: (8),
}));
var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
        span: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_23), false));
__VLS_25.slots.default;
var __VLS_26 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
    field: "code",
    label: "参数标识",
}));
var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
        field: "code",
        label: "参数标识",
    }], __VLS_functionalComponentArgsRest(__VLS_27), false));
__VLS_29.slots.default;
var __VLS_30 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
    modelValue: (__VLS_ctx.modelValue.code),
    placeholder: "请输入参数标识",
}));
var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelValue.code),
        placeholder: "请输入参数标识",
    }], __VLS_functionalComponentArgsRest(__VLS_31), false));
var __VLS_29;
var __VLS_25;
var __VLS_34 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
    span: (8),
}));
var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
        span: (8),
    }], __VLS_functionalComponentArgsRest(__VLS_35), false));
__VLS_37.slots.default;
var __VLS_38 = {}.ARow;
/** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
// @ts-ignore
var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({}));
var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_39), false));
__VLS_41.slots.default;
var __VLS_42 = {}.ACol;
/** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
// @ts-ignore
var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
    span: (12),
}));
var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
        span: (12),
    }], __VLS_functionalComponentArgsRest(__VLS_43), false));
__VLS_45.slots.default;
var __VLS_46 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
    field: "required",
    label: "是否必填",
}));
var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
        field: "required",
        label: "是否必填",
    }], __VLS_functionalComponentArgsRest(__VLS_47), false));
__VLS_49.slots.default;
var __VLS_50 = {}.ASwitch;
/** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
// @ts-ignore
var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
    modelValue: (__VLS_ctx.modelValue.required),
    type: "round",
}));
var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.modelValue.required),
        type: "round",
    }], __VLS_functionalComponentArgsRest(__VLS_51), false));
var __VLS_49;
var __VLS_45;
if (__VLS_ctx.modelValue.type === 'CHOICE') {
    var __VLS_54 = {}.ACol;
    /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
    // @ts-ignore
    var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
        span: (12),
    }));
    var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
            span: (12),
        }], __VLS_functionalComponentArgsRest(__VLS_55), false));
    __VLS_57.slots.default;
    var __VLS_58 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
        field: "required",
        label: "是否多选",
    }));
    var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
            field: "required",
            label: "是否多选",
        }], __VLS_functionalComponentArgsRest(__VLS_59), false));
    __VLS_61.slots.default;
    var __VLS_62 = {}.ASwitch;
    /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
    // @ts-ignore
    var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
        modelValue: (__VLS_ctx.modelValue.multiple),
        type: "round",
    }));
    var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.modelValue.multiple),
            type: "round",
        }], __VLS_functionalComponentArgsRest(__VLS_63), false));
    var __VLS_61;
    var __VLS_57;
}
var __VLS_41;
var __VLS_37;
var __VLS_9;
if (__VLS_ctx.modelValue.type === 'CHOICE') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var _loop_1 = function (item, index) {
        var __VLS_66 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
            field: ("options[".concat(index, "]")),
            label: "可选项",
            hideLabel: (index !== 0),
            rules: ([{ required: true, message: '选项未填写' }]),
        }));
        var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
                field: ("options[".concat(index, "]")),
                label: "可选项",
                hideLabel: (index !== 0),
                rules: ([{ required: true, message: '选项未填写' }]),
            }], __VLS_functionalComponentArgsRest(__VLS_67), false));
        __VLS_69.slots.default;
        var __VLS_70 = {}.AInput;
        /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
        // @ts-ignore
        var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
            modelValue: (__VLS_ctx.modelValue.options[index]),
            placeholder: "请输入选项",
        }));
        var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.modelValue.options[index]),
                placeholder: "请输入选项",
            }], __VLS_functionalComponentArgsRest(__VLS_71), false));
        if (__VLS_ctx.modelValue.options.length > 1) {
            var __VLS_74 = {}.IconMinusCircle;
            /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
            // @ts-ignore
            var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_75), false));
            var __VLS_78 = void 0;
            var __VLS_79 = void 0;
            var __VLS_80 = void 0;
            var __VLS_81 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(__VLS_ctx.modelValue.type === 'CHOICE'))
                        return;
                    if (!(__VLS_ctx.modelValue.options.length > 1))
                        return;
                    __VLS_ctx.handleRemoveOption(index);
                }
            };
        }
    };
    var __VLS_77, __VLS_69;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.modelValue.options)); _i < _a.length; _i++) {
        var _b = _a[_i], item = _b[0], index = _b[1];
        _loop_1(item, index);
    }
    var __VLS_82 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82(__assign(__assign({ 'onClick': {} }, { style: {} }), { size: "small", shape: "circle" })));
    var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { style: {} }), { size: "small", shape: "circle" })], __VLS_functionalComponentArgsRest(__VLS_83), false));
    var __VLS_86 = void 0;
    var __VLS_87 = void 0;
    var __VLS_88 = void 0;
    var __VLS_89 = {
        onClick: (__VLS_ctx.handleAddOption)
    };
    __VLS_85.slots.default;
    var __VLS_90 = {}.IconPlus;
    /** @type {[typeof __VLS_components.IconPlus, typeof __VLS_components.iconPlus, ]} */ ;
    // @ts-ignore
    var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({}));
    var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_91), false));
    var __VLS_85;
}
var __VLS_94 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
    field: "defaultValue",
    label: "默认值",
}));
var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
        field: "defaultValue",
        label: "默认值",
    }], __VLS_functionalComponentArgsRest(__VLS_95), false));
__VLS_97.slots.default;
if (__VLS_ctx.modelValue.type === 'STRING') {
    var __VLS_98 = {}.AInput;
    /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
    // @ts-ignore
    var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
        modelValue: (props.modelValue.defaultValue),
        placeholder: "请输入默认值",
    }));
    var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
            modelValue: (props.modelValue.defaultValue),
            placeholder: "请输入默认值",
        }], __VLS_functionalComponentArgsRest(__VLS_99), false));
}
if (__VLS_ctx.modelValue.type === 'NUMBER') {
    var __VLS_102 = {}.AInputNumber;
    /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
    // @ts-ignore
    var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
        modelValue: (props.modelValue.defaultValue),
        placeholder: "请输入默认值",
    }));
    var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
            modelValue: (props.modelValue.defaultValue),
            placeholder: "请输入默认值",
        }], __VLS_functionalComponentArgsRest(__VLS_103), false));
}
if (__VLS_ctx.modelValue.type === 'BOOLEAN') {
    var __VLS_106 = {}.ASwitch;
    /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
    // @ts-ignore
    var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106({
        modelValue: (__VLS_ctx.modelValue.defaultValue),
        type: "round",
    }));
    var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.modelValue.defaultValue),
            type: "round",
        }], __VLS_functionalComponentArgsRest(__VLS_107), false));
}
if (__VLS_ctx.modelValue.type === 'CHOICE') {
    var __VLS_110 = {}.ASelect;
    /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
    // @ts-ignore
    var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
        allowClear: true,
        multiple: (__VLS_ctx.modelValue.multiple),
        modelValue: (__VLS_ctx.modelValue.defaultValue),
        placeholder: "请选择默认值",
    }));
    var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
            allowClear: true,
            multiple: (__VLS_ctx.modelValue.multiple),
            modelValue: (__VLS_ctx.modelValue.defaultValue),
            placeholder: "请选择默认值",
        }], __VLS_functionalComponentArgsRest(__VLS_111), false));
    __VLS_113.slots.default;
    for (var _c = 0, _d = __VLS_getVForSourceType((__VLS_ctx.modelValue.options.filter(function (item) { return item; }))); _c < _d.length; _c++) {
        var _e = _d[_c], item = _e[0], index = _e[1];
        var __VLS_114 = {}.AOption;
        /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
        // @ts-ignore
        var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
            key: (index),
            value: (item),
        }));
        var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
                key: (index),
                value: (item),
            }], __VLS_functionalComponentArgsRest(__VLS_115), false));
        __VLS_117.slots.default;
        (item);
        var __VLS_117;
    }
    var __VLS_113;
}
if (__VLS_ctx.modelValue.type === 'LIST') {
    var __VLS_118 = {}.ATextarea;
    /** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
    // @ts-ignore
    var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
        modelValue: (__VLS_ctx.modelValue.defaultValue),
        placeholder: "请输入默认值，使用换行区分",
        autoSize: ({
            minRows: 3,
            maxRows: 8,
        }),
    }));
    var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
            modelValue: (__VLS_ctx.modelValue.defaultValue),
            placeholder: "请输入默认值，使用换行区分",
            autoSize: ({
                minRows: 3,
                maxRows: 8,
            }),
        }], __VLS_functionalComponentArgsRest(__VLS_119), false));
}
var __VLS_97;
var __VLS_3;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "right" }));
var __VLS_122 = {}.IconMinusCircleFill;
/** @type {[typeof __VLS_components.IconMinusCircleFill, typeof __VLS_components.iconMinusCircleFill, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122(__assign({ 'onClick': {} }, { style: {} })));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_123), false));
var __VLS_126;
var __VLS_127;
var __VLS_128;
var __VLS_129 = {
    onClick: (__VLS_ctx.handleRemoveParamConfig)
};
var __VLS_125;
/** @type {__VLS_StyleScopedClasses['param-container']} */ ;
/** @type {__VLS_StyleScopedClasses['right']} */ ;
// @ts-ignore
var __VLS_5 = __VLS_4;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            paramConfigRef: paramConfigRef,
            paramConfigRules: paramConfigRules,
            handleAddOption: handleAddOption,
            handleRemoveOption: handleRemoveOption,
            handleRemoveParamConfig: handleRemoveParamConfig,
        };
    },
    __typeEmits: {},
    __typeProps: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
    __typeEmits: {},
    __typeProps: {},
});
; /* PartiallyEnd: #4569/main.vue */
