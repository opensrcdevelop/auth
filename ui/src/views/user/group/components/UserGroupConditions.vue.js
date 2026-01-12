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
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
var _a, _b, _c, _d, _e;
import { getEnabledDictData } from "@/api/dict";
import { getUserAttrs } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { computed, onMounted, ref, watch } from "vue";
var props = defineProps();
var emit = defineEmits();
var columnsLoaded = ref(false);
var formRef = ref();
var groupRef = ref();
var allUserColumns = [];
var allDictDatas = {};
var canRemove = computed(function () {
    var _a, _b;
    return ((_a = props.conditions.filters) === null || _a === void 0 ? void 0 : _a.length) + ((_b = props.conditions.groups) === null || _b === void 0 ? void 0 : _b.length) > 1;
});
watch(function () { return props.conditions; }, function (newVal) {
    allUserColumns.forEach(function (item) { return __awaiter(void 0, void 0, void 0, function () {
        return __generator(this, function (_a) {
            if (item.dataType === "DICT" && item.dictId) {
                if (item.cascadeDict !== undefined) {
                    setFilterCascadeDict(item.key, item.cascadeDict);
                }
            }
            return [2 /*return*/];
        });
    }); });
    emit("update:conditions", newVal);
}, { deep: true, immediate: true });
var handleGetAllUserColumns = function () {
    getUserAttrs({
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            allUserColumns.length = 0;
            allUserColumns.push.apply(allUserColumns, data.list);
            var getEnabledDictDataPromises = [];
            allUserColumns.forEach(function (item) { return __awaiter(void 0, void 0, void 0, function () {
                return __generator(this, function (_a) {
                    if (item.dataType === "DICT" && item.dictId) {
                        if (item.cascadeDict !== undefined) {
                            setFilterCascadeDict(item.key, item.cascadeDict);
                        }
                        allDictDatas[item.key] = [];
                        getEnabledDictDataPromises.push(handleGetEnabledDictData(item.key, item.dictId));
                    }
                    return [2 /*return*/];
                });
            }); });
            if (getEnabledDictDataPromises.length > 0) {
                Promise.all(getEnabledDictDataPromises).then(function () {
                    columnsLoaded.value = true;
                });
                return;
            }
            columnsLoaded.value = true;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户属性");
    });
};
var setFilterCascadeDict = function (key, isCascadeDict) {
    var filter = props.conditions.filters.find(function (item) { return item.key === key; });
    if (filter) {
        filter.cascadeDict = isCascadeDict;
    }
};
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
var handleUserColumnsSelectChange = function (value) {
    var column = allUserColumns.find(function (item) { return item.key === value; });
    if (column) {
        var filter = props.conditions.filters.find(function (item) { return item.key === column.key; });
        filter.dataType = column.dataType;
        filter.extFlg = column.extFlg;
        filter.cascadeDict = column.cascadeDict;
        filter.value = undefined;
        filter.filterType = undefined;
    }
};
var handleAddFilter = function () {
    props.conditions.filters.push({
        key: undefined,
        dataType: "STRING",
        value: undefined,
        filterType: undefined,
        extFlg: undefined,
        cascadeDict: undefined,
    });
};
var handleRemoveFilter = function (index) {
    props.conditions.filters.splice(index, 1);
};
var handleSwapperConjunction = function () {
    props.conditions.conjunction =
        props.conditions.conjunction === "AND" ? "OR" : "AND";
};
var handleAddGroup = function () {
    if (!props.conditions.groups) {
        props.conditions.groups = [];
    }
    props.conditions.groups.push({
        conjunction: "AND",
        filters: [
            {
                key: undefined,
                dataType: "STRING",
                value: undefined,
                filterType: undefined,
                extFlg: undefined,
                cascadeDict: undefined,
            },
        ],
        groups: [],
    });
};
var handleRemoveGroup = function (index) {
    props.conditions.groups.splice(index, 1);
};
var handleValidateForm = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateRes, groupValidateRes, groupValidateResArr_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, formRef.value.validate()];
            case 1:
                validateRes = _a.sent();
                groupValidateRes = true;
                if (!groupRef.value) return [3 /*break*/, 3];
                groupValidateResArr_1 = [];
                groupRef.value.forEach(function (item) { return groupValidateResArr_1.push(item.validate()); });
                return [4 /*yield*/, Promise.all(groupValidateResArr_1)];
            case 2:
                groupValidateRes = (_a.sent()).every(function (item) { return item; });
                _a.label = 3;
            case 3: return [2 /*return*/, validateRes === undefined && groupValidateRes];
        }
    });
}); };
onMounted(function () {
    if (!props.columns || props.columns.length === 0) {
        handleGetAllUserColumns();
    }
    else {
        allUserColumns.push.apply(allUserColumns, props.columns);
        if (props.dictDatas) {
            Object.assign(allDictDatas, props.dictDatas);
        }
        columnsLoaded.value = true;
    }
});
var __VLS_exposed = {
    validate: handleValidateForm,
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
/** @type {__VLS_StyleScopedClasses['conjunction-line-top']} */ ;
/** @type {__VLS_StyleScopedClasses['conjunction-line-btm']} */ ;
// CSS variable injection 
// CSS variable injection end 
if (__VLS_ctx.columnsLoaded) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "conditions-container" }));
    if (((_a = __VLS_ctx.conditions.filters) === null || _a === void 0 ? void 0 : _a.length) + ((_b = __VLS_ctx.conditions.groups) === null || _b === void 0 ? void 0 : _b.length) > 1) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "conjunction-container" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "conjunction-line-top" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.handleSwapperConjunction) }, { class: "conjunction" }));
        var __VLS_0 = {}.ASpace;
        /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
        // @ts-ignore
        var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
        var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_1), false));
        __VLS_3.slots.default;
        if (__VLS_ctx.conditions.conjunction === 'AND') {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        }
        if (__VLS_ctx.conditions.conjunction === 'OR') {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)({});
        }
        var __VLS_4 = {}.IconSwap;
        /** @type {[typeof __VLS_components.IconSwap, typeof __VLS_components.iconSwap, ]} */ ;
        // @ts-ignore
        var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({}));
        var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_5), false));
        var __VLS_3;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "conjunction-line-btm" }));
    }
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "conditions-content-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    var __VLS_8 = {}.AForm;
    /** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
    // @ts-ignore
    var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({
        model: (props.conditions),
        ref: "formRef",
    }));
    var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{
            model: (props.conditions),
            ref: "formRef",
        }], __VLS_functionalComponentArgsRest(__VLS_9), false));
    /** @type {typeof __VLS_ctx.formRef} */ ;
    var __VLS_12 = {};
    __VLS_11.slots.default;
    var _loop_1 = function (filter, index) {
        var __VLS_14 = {}.ARow;
        /** @type {[typeof __VLS_components.ARow, typeof __VLS_components.aRow, typeof __VLS_components.ARow, typeof __VLS_components.aRow, ]} */ ;
        // @ts-ignore
        var __VLS_15 = __VLS_asFunctionalComponent(__VLS_14, new __VLS_14({
            gutter: (8),
            key: (index),
        }));
        var __VLS_16 = __VLS_15.apply(void 0, __spreadArray([{
                gutter: (8),
                key: (index),
            }], __VLS_functionalComponentArgsRest(__VLS_15), false));
        __VLS_17.slots.default;
        var __VLS_18 = {}.ACol;
        /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
        // @ts-ignore
        var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18({
            span: (8),
        }));
        var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([{
                span: (8),
            }], __VLS_functionalComponentArgsRest(__VLS_19), false));
        __VLS_21.slots.default;
        var __VLS_22 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_23 = __VLS_asFunctionalComponent(__VLS_22, new __VLS_22({
            field: ("filters[".concat(index, "].key")),
            hideLabel: true,
            rules: ([{ required: true, message: '字段未选择' }]),
        }));
        var __VLS_24 = __VLS_23.apply(void 0, __spreadArray([{
                field: ("filters[".concat(index, "].key")),
                hideLabel: true,
                rules: ([{ required: true, message: '字段未选择' }]),
            }], __VLS_functionalComponentArgsRest(__VLS_23), false));
        __VLS_25.slots.default;
        var __VLS_26 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_27 = __VLS_asFunctionalComponent(__VLS_26, new __VLS_26(__assign({ 'onChange': {} }, { placeholder: "请选择字段", modelValue: (filter.key), allowSearch: true })));
        var __VLS_28 = __VLS_27.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { placeholder: "请选择字段", modelValue: (filter.key), allowSearch: true })], __VLS_functionalComponentArgsRest(__VLS_27), false));
        var __VLS_30 = void 0;
        var __VLS_31 = void 0;
        var __VLS_32 = void 0;
        var __VLS_33 = {
            onChange: (__VLS_ctx.handleUserColumnsSelectChange)
        };
        __VLS_29.slots.default;
        var _loop_3 = function (column) {
            var __VLS_34 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_35 = __VLS_asFunctionalComponent(__VLS_34, new __VLS_34({
                key: (column.key),
                value: (column.key),
                disabled: (__VLS_ctx.conditions.filters.some(function (item) { return item.key === column.key; })),
            }));
            var __VLS_36 = __VLS_35.apply(void 0, __spreadArray([{
                    key: (column.key),
                    value: (column.key),
                    disabled: (__VLS_ctx.conditions.filters.some(function (item) { return item.key === column.key; })),
                }], __VLS_functionalComponentArgsRest(__VLS_35), false));
            __VLS_37.slots.default;
            (column.name);
        };
        for (var _l = 0, _m = __VLS_getVForSourceType((__VLS_ctx.allUserColumns)); _l < _m.length; _l++) {
            var column = _m[_l][0];
            _loop_3(column);
        }
        var __VLS_38 = {}.ACol;
        /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
        // @ts-ignore
        var __VLS_39 = __VLS_asFunctionalComponent(__VLS_38, new __VLS_38({
            span: (6),
        }));
        var __VLS_40 = __VLS_39.apply(void 0, __spreadArray([{
                span: (6),
            }], __VLS_functionalComponentArgsRest(__VLS_39), false));
        __VLS_41.slots.default;
        var __VLS_42 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
            field: ("filters[".concat(index, "].filterType")),
            hideLabel: true,
            rules: ([{ required: true, message: '运算符未选择' }]),
        }));
        var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
                field: ("filters[".concat(index, "].filterType")),
                hideLabel: true,
                rules: ([{ required: true, message: '运算符未选择' }]),
            }], __VLS_functionalComponentArgsRest(__VLS_43), false));
        __VLS_45.slots.default;
        var __VLS_46 = {}.ASelect;
        /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
        // @ts-ignore
        var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
            placeholder: "请选择运算符",
            modelValue: (filter.filterType),
        }));
        var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
                placeholder: "请选择运算符",
                modelValue: (filter.filterType),
            }], __VLS_functionalComponentArgsRest(__VLS_47), false));
        __VLS_49.slots.default;
        if ([
            'STRING',
            'NUMBER',
            'BOOLEAN',
            'DATETIME',
            'DATE',
            'DICT',
        ].includes(filter.dataType)) {
            var __VLS_50 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50({
                value: "EQ",
            }));
            var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([{
                    value: "EQ",
                }], __VLS_functionalComponentArgsRest(__VLS_51), false));
            __VLS_53.slots.default;
        }
        if ([
            'STRING',
            'NUMBER',
            'BOOLEAN',
            'DATETIME',
            'DATE',
            'DICT',
        ].includes(filter.dataType)) {
            var __VLS_54 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({
                value: "NE",
            }));
            var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{
                    value: "NE",
                }], __VLS_functionalComponentArgsRest(__VLS_55), false));
            __VLS_57.slots.default;
        }
        if (['STRING'].includes(filter.dataType)) {
            var __VLS_58 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({
                value: "LIKE",
            }));
            var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{
                    value: "LIKE",
                }], __VLS_functionalComponentArgsRest(__VLS_59), false));
            __VLS_61.slots.default;
        }
        if (['STRING'].includes(filter.dataType)) {
            var __VLS_62 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62({
                value: "NOT_LIKE",
            }));
            var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([{
                    value: "NOT_LIKE",
                }], __VLS_functionalComponentArgsRest(__VLS_63), false));
            __VLS_65.slots.default;
        }
        if (['NUMBER', 'DATETIME', 'DATE'].includes(filter.dataType)) {
            var __VLS_66 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_67 = __VLS_asFunctionalComponent(__VLS_66, new __VLS_66({
                value: "GT",
            }));
            var __VLS_68 = __VLS_67.apply(void 0, __spreadArray([{
                    value: "GT",
                }], __VLS_functionalComponentArgsRest(__VLS_67), false));
            __VLS_69.slots.default;
        }
        if (['NUMBER', 'DATETIME', 'DATE'].includes(filter.dataType)) {
            var __VLS_70 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_71 = __VLS_asFunctionalComponent(__VLS_70, new __VLS_70({
                value: "LT",
            }));
            var __VLS_72 = __VLS_71.apply(void 0, __spreadArray([{
                    value: "LT",
                }], __VLS_functionalComponentArgsRest(__VLS_71), false));
            __VLS_73.slots.default;
        }
        var __VLS_74 = {}.ACol;
        /** @type {[typeof __VLS_components.ACol, typeof __VLS_components.aCol, typeof __VLS_components.ACol, typeof __VLS_components.aCol, ]} */ ;
        // @ts-ignore
        var __VLS_75 = __VLS_asFunctionalComponent(__VLS_74, new __VLS_74({
            span: (10),
        }));
        var __VLS_76 = __VLS_75.apply(void 0, __spreadArray([{
                span: (10),
            }], __VLS_functionalComponentArgsRest(__VLS_75), false));
        __VLS_77.slots.default;
        var __VLS_78 = {}.AFormItem;
        /** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
        // @ts-ignore
        var __VLS_79 = __VLS_asFunctionalComponent(__VLS_78, new __VLS_78({
            field: ("filters[".concat(index, "].value")),
            hideLabel: true,
            rules: ([{ required: true, message: '未输入 / 选择' }]),
        }));
        var __VLS_80 = __VLS_79.apply(void 0, __spreadArray([{
                field: ("filters[".concat(index, "].value")),
                hideLabel: true,
                rules: ([{ required: true, message: '未输入 / 选择' }]),
            }], __VLS_functionalComponentArgsRest(__VLS_79), false));
        __VLS_81.slots.default;
        if (filter.dataType === 'NUMBER') {
            var __VLS_82 = {}.AInputNumber;
            /** @type {[typeof __VLS_components.AInputNumber, typeof __VLS_components.aInputNumber, ]} */ ;
            // @ts-ignore
            var __VLS_83 = __VLS_asFunctionalComponent(__VLS_82, new __VLS_82({
                hideButton: true,
                modelValue: (filter.value),
                placeholder: "请输入",
            }));
            var __VLS_84 = __VLS_83.apply(void 0, __spreadArray([{
                    hideButton: true,
                    modelValue: (filter.value),
                    placeholder: "请输入",
                }], __VLS_functionalComponentArgsRest(__VLS_83), false));
        }
        if (filter.dataType === 'STRING') {
            var __VLS_86 = {}.AInput;
            /** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
            // @ts-ignore
            var __VLS_87 = __VLS_asFunctionalComponent(__VLS_86, new __VLS_86({
                modelValue: (filter.value),
                placeholder: "请输入",
            }));
            var __VLS_88 = __VLS_87.apply(void 0, __spreadArray([{
                    modelValue: (filter.value),
                    placeholder: "请输入",
                }], __VLS_functionalComponentArgsRest(__VLS_87), false));
        }
        if (filter.dataType === 'BOOLEAN') {
            var __VLS_90 = {}.ASelect;
            /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
            // @ts-ignore
            var __VLS_91 = __VLS_asFunctionalComponent(__VLS_90, new __VLS_90({
                modelValue: (filter.value),
                placeholder: "请选择",
            }));
            var __VLS_92 = __VLS_91.apply(void 0, __spreadArray([{
                    modelValue: (filter.value),
                    placeholder: "请选择",
                }], __VLS_functionalComponentArgsRest(__VLS_91), false));
            __VLS_93.slots.default;
            var __VLS_94 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_95 = __VLS_asFunctionalComponent(__VLS_94, new __VLS_94({
                value: "true",
            }));
            var __VLS_96 = __VLS_95.apply(void 0, __spreadArray([{
                    value: "true",
                }], __VLS_functionalComponentArgsRest(__VLS_95), false));
            __VLS_97.slots.default;
            var __VLS_98 = {}.AOption;
            /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
            // @ts-ignore
            var __VLS_99 = __VLS_asFunctionalComponent(__VLS_98, new __VLS_98({
                value: "false",
            }));
            var __VLS_100 = __VLS_99.apply(void 0, __spreadArray([{
                    value: "false",
                }], __VLS_functionalComponentArgsRest(__VLS_99), false));
            __VLS_101.slots.default;
        }
        if (filter.dataType === 'DATETIME') {
            var __VLS_102 = {}.ADatePicker;
            /** @type {[typeof __VLS_components.ADatePicker, typeof __VLS_components.aDatePicker, ]} */ ;
            // @ts-ignore
            var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102(__assign({ style: {} }, { showTime: true, valueFormat: "timestamp", modelValue: (filter.value) })));
            var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([__assign({ style: {} }, { showTime: true, valueFormat: "timestamp", modelValue: (filter.value) })], __VLS_functionalComponentArgsRest(__VLS_103), false));
        }
        if (filter.dataType === 'DATE') {
            var __VLS_106 = {}.ADatePicker;
            /** @type {[typeof __VLS_components.ADatePicker, typeof __VLS_components.aDatePicker, ]} */ ;
            // @ts-ignore
            var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106(__assign({ style: {} }, { valueFormat: "timestamp", modelValue: (filter.value) })));
            var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([__assign({ style: {} }, { valueFormat: "timestamp", modelValue: (filter.value) })], __VLS_functionalComponentArgsRest(__VLS_107), false));
        }
        if (filter.dataType === 'DICT' && !filter.cascadeDict) {
            var __VLS_110 = {}.ASelect;
            /** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
            // @ts-ignore
            var __VLS_111 = __VLS_asFunctionalComponent(__VLS_110, new __VLS_110({
                modelValue: (filter.value),
                placeholder: "请选择",
            }));
            var __VLS_112 = __VLS_111.apply(void 0, __spreadArray([{
                    modelValue: (filter.value),
                    placeholder: "请选择",
                }], __VLS_functionalComponentArgsRest(__VLS_111), false));
            __VLS_113.slots.default;
            for (var _o = 0, _p = __VLS_getVForSourceType((__VLS_ctx.allDictDatas[filter.key])); _o < _p.length; _o++) {
                var dictData = _p[_o][0];
                var __VLS_114 = {}.AOption;
                /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
                // @ts-ignore
                var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
                    value: (dictData.id),
                    key: (dictData.id),
                }));
                var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
                        value: (dictData.id),
                        key: (dictData.id),
                    }], __VLS_functionalComponentArgsRest(__VLS_115), false));
                __VLS_117.slots.default;
                (dictData.label);
            }
        }
        if (filter.dataType === 'DICT' && filter.cascadeDict) {
            var __VLS_118 = {}.ACascader;
            /** @type {[typeof __VLS_components.ACascader, typeof __VLS_components.aCascader, ]} */ ;
            // @ts-ignore
            var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
                modelValue: (filter.value),
                placeholder: "请选择",
                expandTrigger: "hover",
                options: (__VLS_ctx.allDictDatas[filter.key]),
                fieldNames: ({ value: 'id', label: 'label' }),
            }));
            var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
                    modelValue: (filter.value),
                    placeholder: "请选择",
                    expandTrigger: "hover",
                    options: (__VLS_ctx.allDictDatas[filter.key]),
                    fieldNames: ({ value: 'id', label: 'label' }),
                }], __VLS_functionalComponentArgsRest(__VLS_119), false));
        }
        if (__VLS_ctx.canRemove) {
            var __VLS_122 = {}.IconMinusCircle;
            /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
            // @ts-ignore
            var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122(__assign({ 'onClick': {} }, { class: "remove-filter" })));
            var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { class: "remove-filter" })], __VLS_functionalComponentArgsRest(__VLS_123), false));
            var __VLS_126 = void 0;
            var __VLS_127 = void 0;
            var __VLS_128 = void 0;
            var __VLS_129 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    if (!(__VLS_ctx.columnsLoaded))
                        return;
                    if (!(__VLS_ctx.canRemove))
                        return;
                    __VLS_ctx.handleRemoveFilter(index);
                }
            };
        }
    };
    var __VLS_37, __VLS_29, __VLS_25, __VLS_21, __VLS_53, __VLS_57, __VLS_61, __VLS_65, __VLS_69, __VLS_73, __VLS_49, __VLS_45, __VLS_41, __VLS_97, __VLS_101, __VLS_93, __VLS_117, __VLS_113, __VLS_125, __VLS_81, __VLS_77, __VLS_17;
    for (var _i = 0, _f = __VLS_getVForSourceType(((_c = props.conditions) === null || _c === void 0 ? void 0 : _c.filters)); _i < _f.length; _i++) {
        var _g = _f[_i], filter = _g[0], index = _g[1];
        _loop_1(filter, index);
    }
    var __VLS_11;
    if (__VLS_ctx.conditions.groups.length > 0) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
        var _loop_2 = function (group, index) {
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({
                key: (index),
            });
            __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "group" }));
            var __VLS_130 = {}.UserGroupConditions;
            /** @type {[typeof __VLS_components.UserGroupConditions, ]} */ ;
            // @ts-ignore
            var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130(__assign({ class: "group-item" }, { conditions: (group), columns: (__VLS_ctx.allUserColumns), dictDatas: (__VLS_ctx.allDictDatas), ref: "groupRef" })));
            var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([__assign({ class: "group-item" }, { conditions: (group), columns: (__VLS_ctx.allUserColumns), dictDatas: (__VLS_ctx.allDictDatas), ref: "groupRef" })], __VLS_functionalComponentArgsRest(__VLS_131), false));
            /** @type {typeof __VLS_ctx.groupRef} */ ;
            __VLS_134 = {};
            if (__VLS_ctx.canRemove) {
                var __VLS_136 = {}.IconMinusCircle;
                /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
                // @ts-ignore
                var __VLS_137 = __VLS_asFunctionalComponent(__VLS_136, new __VLS_136(__assign({ 'onClick': {} }, { class: "remove-group" })));
                var __VLS_138 = __VLS_137.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { class: "remove-group" })], __VLS_functionalComponentArgsRest(__VLS_137), false));
                var __VLS_140 = void 0;
                var __VLS_141 = void 0;
                var __VLS_142 = void 0;
                var __VLS_143 = {
                    onClick: function () {
                        var _a = [];
                        for (var _i = 0; _i < arguments.length; _i++) {
                            _a[_i] = arguments[_i];
                        }
                        var $event = _a[0];
                        if (!(__VLS_ctx.columnsLoaded))
                            return;
                        if (!(__VLS_ctx.conditions.groups.length > 0))
                            return;
                        if (!(__VLS_ctx.canRemove))
                            return;
                        __VLS_ctx.handleRemoveGroup(index);
                    }
                };
            }
        };
        var __VLS_134, __VLS_133, __VLS_139;
        for (var _h = 0, _j = __VLS_getVForSourceType((__VLS_ctx.conditions.groups)); _h < _j.length; _h++) {
            var _k = _j[_h], group = _k[0], index = _k[1];
            _loop_2(group, index);
        }
    }
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: ([
            'btn-container',
            ((_d = __VLS_ctx.conditions.filters) === null || _d === void 0 ? void 0 : _d.length) + ((_e = __VLS_ctx.conditions.groups) === null || _e === void 0 ? void 0 : _e.length) > 1
                ? 'btn-container-ml44'
                : '',
        ]) }));
    var __VLS_144 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_145 = __VLS_asFunctionalComponent(__VLS_144, new __VLS_144({}));
    var __VLS_146 = __VLS_145.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_145), false));
    __VLS_147.slots.default;
    var __VLS_148 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_149 = __VLS_asFunctionalComponent(__VLS_148, new __VLS_148(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_150 = __VLS_149.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_149), false));
    var __VLS_152 = void 0;
    var __VLS_153 = void 0;
    var __VLS_154 = void 0;
    var __VLS_155 = {
        onClick: (__VLS_ctx.handleAddFilter)
    };
    __VLS_151.slots.default;
    var __VLS_151;
    var __VLS_156 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_157 = __VLS_asFunctionalComponent(__VLS_156, new __VLS_156(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_158 = __VLS_157.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_157), false));
    var __VLS_160 = void 0;
    var __VLS_161 = void 0;
    var __VLS_162 = void 0;
    var __VLS_163 = {
        onClick: (__VLS_ctx.handleAddGroup)
    };
    __VLS_159.slots.default;
    var __VLS_159;
    var __VLS_147;
}
/** @type {__VLS_StyleScopedClasses['conditions-container']} */ ;
/** @type {__VLS_StyleScopedClasses['conjunction-container']} */ ;
/** @type {__VLS_StyleScopedClasses['conjunction-line-top']} */ ;
/** @type {__VLS_StyleScopedClasses['conjunction']} */ ;
/** @type {__VLS_StyleScopedClasses['conjunction-line-btm']} */ ;
/** @type {__VLS_StyleScopedClasses['conditions-content-container']} */ ;
/** @type {__VLS_StyleScopedClasses['remove-filter']} */ ;
/** @type {__VLS_StyleScopedClasses['group']} */ ;
/** @type {__VLS_StyleScopedClasses['group-item']} */ ;
/** @type {__VLS_StyleScopedClasses['remove-group']} */ ;
/** @type {__VLS_StyleScopedClasses['btn-container']} */ ;
// @ts-ignore
var __VLS_13 = __VLS_12, __VLS_135 = __VLS_134;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            columnsLoaded: columnsLoaded,
            formRef: formRef,
            groupRef: groupRef,
            allUserColumns: allUserColumns,
            allDictDatas: allDictDatas,
            canRemove: canRemove,
            handleUserColumnsSelectChange: handleUserColumnsSelectChange,
            handleAddFilter: handleAddFilter,
            handleRemoveFilter: handleRemoveFilter,
            handleSwapperConjunction: handleSwapperConjunction,
            handleAddGroup: handleAddGroup,
            handleRemoveGroup: handleRemoveGroup,
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
