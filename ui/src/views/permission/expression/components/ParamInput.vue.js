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
import { reactive, ref } from "vue";
var props = defineProps();
var paramsForm = reactive({
    params: props.modelValue,
});
var paramsFormRef = ref(null);
var validate = function () {
    return paramsFormRef.value.validate();
};
var reset = function () {
    paramsFormRef.value.resetFields();
};
var __VLS_exposed = {
    validate: validate,
    reset: reset
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "param-container" }));
var __VLS_0 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
    model: (__VLS_ctx.paramsForm),
    ref: "paramsFormRef",
    layout: "vertical",
}));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.paramsForm),
        ref: "paramsFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_1), false));
/** @type {typeof __VLS_ctx.paramsFormRef} */ ;
var __VLS_4 = {};
__VLS_3.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.configs)); _i < _a.length; _i++) {
    var _b = _a[_i], config = _b[0], index = _b[1];
    var __VLS_6 = {}.AFormItem;
    /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
    // @ts-ignore
    var __VLS_7 = __VLS_asFunctionalComponent(__VLS_6, new __VLS_6({
        key: (index),
        label: (config.name),
        field: ("params[".concat(index, "].value")),
        rules: ([
            { required: config.required, message: "".concat(config.name, "\u662F\u5FC5\u586B\u9879") },
        ]),
    }));
    var __VLS_8 = __VLS_7.apply(void 0, __spreadArray([{
            key: (index),
            label: (config.name),
            field: ("params[".concat(index, "].value")),
            rules: ([
                { required: config.required, message: "".concat(config.name, "\u662F\u5FC5\u586B\u9879") },
            ]),
        }], __VLS_functionalComponentArgsRest(__VLS_7), false));
    __VLS_9.slots.default;
    if (config.type === 'STRING') {
        var __VLS_10 = {}.AInput;
        /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
        // @ts-ignore
        var __VLS_11 = __VLS_asFunctionalComponent(__VLS_10, new __VLS_10({
            modelValue: (__VLS_ctx.modelValue[index].value),
            placeholder: ("\u8BF7\u8F93\u5165".concat(config.name)),
        }));
        var __VLS_12 = __VLS_11.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.modelValue[index].value),
                placeholder: ("\u8BF7\u8F93\u5165".concat(config.name)),
            }], __VLS_functionalComponentArgsRest(__VLS_11), false));
    }
    if (config.type === 'NUMBER') {
        var __VLS_14 = {}.AInputNumber;
        /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
        // @ts-ignore
        var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
            modelValue: (__VLS_ctx.modelValue[index].value),
            placeholder: ("\u8BF7\u8F93\u5165".concat(config.name)),
        }));
        var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.modelValue[index].value),
                placeholder: ("\u8BF7\u8F93\u5165".concat(config.name)),
            }], __VLS_functionalComponentArgsRest(__VLS_15), false));
    }
    if (config.type === 'BOOLEAN') {
        var __VLS_18 = {}.ASwitch;
        /** @type {[typeof __VLS_components.ASwitch, typeof __VLS_components.aSwitch, ]} */ ;
        // @ts-ignore
        var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
            modelValue: (__VLS_ctx.modelValue[index].value),
            type: "round",
        }));
        var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.modelValue[index].value),
                type: "round",
            }], __VLS_functionalComponentArgsRest(__VLS_19), false));
    }
    if (config.type === 'CHOICE') {
        var __VLS_22 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
            allowClear: true,
            multiple: (config.multiple),
            modelValue: (__VLS_ctx.modelValue[index].value),
            placeholder: ("\u8BF7\u9009\u62E9".concat(config.name)),
        }));
        var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
                allowClear: true,
                multiple: (config.multiple),
                modelValue: (__VLS_ctx.modelValue[index].value),
                placeholder: ("\u8BF7\u9009\u62E9".concat(config.name)),
            }], __VLS_functionalComponentArgsRest(__VLS_23), false));
        __VLS_25.slots.default;
        for (var _c = 0, _d = __VLS_getVForSourceType((config.options)); _c < _d.length; _c++) {
            var _e = _d[_c], option = _e[0], index_1 = _e[1];
            var __VLS_26 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26({
                key: (index_1),
                value: (option),
            }));
            var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([{
                    key: (index_1),
                    value: (option),
                }], __VLS_functionalComponentArgsRest(__VLS_27), false));
            __VLS_29.slots.default;
            (option);
            var __VLS_29;
        }
        var __VLS_25;
    }
    if (config.type === 'LIST') {
        var __VLS_30 = {}.ATextarea;
        /** @type {[typeof __VLS_components.ATextarea, typeof __VLS_components.aTextarea, ]} */ ;
        // @ts-ignore
        var __VLS_31 = __VLS_asFunctionalComponent(__VLS_30, new __VLS_30({
            modelValue: (__VLS_ctx.modelValue[index].value),
            placeholder: ("\u8BF7\u8F93\u5165".concat(config.name, "\uFF0C\u4F7F\u7528\u6362\u884C\u533A\u5206")),
            autoSize: ({
                minRows: 3,
                maxRows: 8,
            }),
        }));
        var __VLS_32 = __VLS_31.apply(void 0, __spreadArray([{
                modelValue: (__VLS_ctx.modelValue[index].value),
                placeholder: ("\u8BF7\u8F93\u5165".concat(config.name, "\uFF0C\u4F7F\u7528\u6362\u884C\u533A\u5206")),
                autoSize: ({
                    minRows: 3,
                    maxRows: 8,
                }),
            }], __VLS_functionalComponentArgsRest(__VLS_31), false));
    }
    var __VLS_9;
}
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['param-container']} */ ;
// @ts-ignore
var __VLS_5 = __VLS_4;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            paramsForm: paramsForm,
            paramsFormRef: paramsFormRef,
        };
    },
    __typeProps: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
    __typeProps: {},
});
; /* PartiallyEnd: #4569/main.vue */
