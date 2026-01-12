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
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "dict-header" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "left" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "info" }));
var __VLS_0 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ 'onClick': {} }, { type: "primary" })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "primary" })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onClick: (__VLS_ctx.handleToCreateDict)
};
__VLS_3.slots.default;
var __VLS_3;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "dict-search" }));
var __VLS_8 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { modelValue: (__VLS_ctx.dictSerachKeyword) }), { style: ({ width: '320px' }) }), { placeholder: "输入字典名称或标识进行搜索", allowClear: true })));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { modelValue: (__VLS_ctx.dictSerachKeyword) }), { style: ({ width: '320px' }) }), { placeholder: "输入字典名称或标识进行搜索", allowClear: true })], __VLS_functionalComponentArgsRest(__VLS_9), false));
var __VLS_12;
var __VLS_13;
var __VLS_14;
var __VLS_15 = {
    onSearch: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetDictList(1, 15);
    }
};
var __VLS_16 = {
    onKeyup: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetDictList(1, 15);
    }
};
var __VLS_17 = {
    onClear: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.handleGetDictList(1, 15);
    }
};
var __VLS_11;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "dict-list" }));
var __VLS_18 = {}.ATable;
/** @type {[typeof __VLS_components.ATable, typeof __VLS_components.aTable, typeof __VLS_components.ATable, typeof __VLS_components.aTable, ]} */ ;
// @ts-ignore
var __VLS_19 = __VLS_asFunctionalComponent(__VLS_18, new __VLS_18(__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.dictList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.dictPagination.pagination), rowKey: "uniqueKey" })));
var __VLS_20 = __VLS_19.apply(void 0, __spreadArray([__assign(__assign({ 'onPageChange': {} }, { 'onPageSizeChange': {} }), { data: (__VLS_ctx.dictList), bordered: (false), scroll: ({ y: '100%' }), pagination: (__VLS_ctx.dictPagination.pagination), rowKey: "uniqueKey" })], __VLS_functionalComponentArgsRest(__VLS_19), false));
var __VLS_22;
var __VLS_23;
var __VLS_24;
var __VLS_25 = {
    onPageChange: (__VLS_ctx.dictPagination.handlePageChange)
};
var __VLS_26 = {
    onPageSizeChange: (__VLS_ctx.dictPagination.handlePageSizeChange)
};
__VLS_21.slots.default;
{
    var __VLS_thisSlot = __VLS_21.slots.columns;
    var __VLS_27 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_28 = __VLS_asFunctionalComponent(__VLS_27, new __VLS_27({
        title: "字典名称",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_29 = __VLS_28.apply(void 0, __spreadArray([{
            title: "字典名称",
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
                __VLS_ctx.handleToDictDetail(record_1);
            } }, { class: "table-column-dictname" }));
        (record_1.name);
    }
    var __VLS_30;
    var __VLS_31 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_32 = __VLS_asFunctionalComponent(__VLS_31, new __VLS_31({
        title: "字典标识",
        ellipsis: true,
        tooltip: true,
        sortable: ({
            sortDirections: ['ascend', 'descend'],
        }),
    }));
    var __VLS_33 = __VLS_32.apply(void 0, __spreadArray([{
            title: "字典标识",
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
        (record.code);
    }
    var __VLS_34;
    var __VLS_35 = {}.ATableColumn;
    /** @type {[typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, typeof __VLS_components.ATableColumn, typeof __VLS_components.aTableColumn, ]} */ ;
    // @ts-ignore
    var __VLS_36 = __VLS_asFunctionalComponent(__VLS_35, new __VLS_35({
        title: "操作",
        width: (60),
    }));
    var __VLS_37 = __VLS_36.apply(void 0, __spreadArray([{
            title: "操作",
            width: (60),
        }], __VLS_functionalComponentArgsRest(__VLS_36), false));
    __VLS_38.slots.default;
    {
        var __VLS_thisSlot_3 = __VLS_38.slots.cell;
        var record_2 = __VLS_getSlotParams(__VLS_thisSlot_3)[0].record;
        var __VLS_39 = {}.ADropdown;
        /** @type {[typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, typeof __VLS_components.ADropdown, typeof __VLS_components.aDropdown, ]} */ ;
        // @ts-ignore
        var __VLS_40 = __VLS_asFunctionalComponent(__VLS_39, new __VLS_39({}));
        var __VLS_41 = __VLS_40.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_40), false));
        __VLS_42.slots.default;
        var __VLS_43 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_44 = __VLS_asFunctionalComponent(__VLS_43, new __VLS_43({
            type: "text",
        }));
        var __VLS_45 = __VLS_44.apply(void 0, __spreadArray([{
                type: "text",
            }], __VLS_functionalComponentArgsRest(__VLS_44), false));
        __VLS_46.slots.default;
        {
            var __VLS_thisSlot_4 = __VLS_46.slots.icon;
            var __VLS_47 = {}.IconMore;
            /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
            // @ts-ignore
            var __VLS_48 = __VLS_asFunctionalComponent(__VLS_47, new __VLS_47({}));
            var __VLS_49 = __VLS_48.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_48), false));
        }
        var __VLS_46;
        {
            var __VLS_thisSlot_5 = __VLS_42.slots.content;
            var __VLS_51 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_52 = __VLS_asFunctionalComponent(__VLS_51, new __VLS_51(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_53 = __VLS_52.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_52), false));
            var __VLS_55 = void 0;
            var __VLS_56 = void 0;
            var __VLS_57 = void 0;
            var __VLS_58 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleAddChildDict(record_2.id);
                }
            };
            __VLS_54.slots.default;
            {
                var __VLS_thisSlot_6 = __VLS_54.slots.icon;
                var __VLS_59 = {}.IconPlusCircle;
                /** @type {[typeof __VLS_components.IconPlusCircle, typeof __VLS_components.iconPlusCircle, ]} */ ;
                // @ts-ignore
                var __VLS_60 = __VLS_asFunctionalComponent(__VLS_59, new __VLS_59({}));
                var __VLS_61 = __VLS_60.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_60), false));
            }
            var __VLS_54;
            if ((record_2 === null || record_2 === void 0 ? void 0 : record_2.level) > 1) {
                var __VLS_63 = {}.ADoption;
                /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
                // @ts-ignore
                var __VLS_64 = __VLS_asFunctionalComponent(__VLS_63, new __VLS_63(__assign({ 'onClick': {} }, { style: {} })));
                var __VLS_65 = __VLS_64.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_64), false));
                var __VLS_67 = void 0;
                var __VLS_68 = void 0;
                var __VLS_69 = void 0;
                var __VLS_70 = {
                    onClick: function () {
                        var _a = [];
                        for (var _i = 0; _i < arguments.length; _i++) {
                            _a[_i] = arguments[_i];
                        }
                        var $event = _a[0];
                        if (!((record_2 === null || record_2 === void 0 ? void 0 : record_2.level) > 1))
                            return;
                        __VLS_ctx.handleRemoveChildDict(record_2);
                    }
                };
                __VLS_66.slots.default;
                {
                    var __VLS_thisSlot_7 = __VLS_66.slots.icon;
                    var __VLS_71 = {}.IconMinusCircle;
                    /** @type {[typeof __VLS_components.IconMinusCircle, typeof __VLS_components.iconMinusCircle, ]} */ ;
                    // @ts-ignore
                    var __VLS_72 = __VLS_asFunctionalComponent(__VLS_71, new __VLS_71({}));
                    var __VLS_73 = __VLS_72.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_72), false));
                }
                var __VLS_66;
            }
            var __VLS_75 = {}.ADoption;
            /** @type {[typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, typeof __VLS_components.ADoption, typeof __VLS_components.aDoption, ]} */ ;
            // @ts-ignore
            var __VLS_76 = __VLS_asFunctionalComponent(__VLS_75, new __VLS_75(__assign({ 'onClick': {} }, { style: {} })));
            var __VLS_77 = __VLS_76.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { style: {} })], __VLS_functionalComponentArgsRest(__VLS_76), false));
            var __VLS_79 = void 0;
            var __VLS_80 = void 0;
            var __VLS_81 = void 0;
            var __VLS_82 = {
                onClick: function () {
                    var _a = [];
                    for (var _i = 0; _i < arguments.length; _i++) {
                        _a[_i] = arguments[_i];
                    }
                    var $event = _a[0];
                    __VLS_ctx.handleDeleteDict(record_2);
                }
            };
            __VLS_78.slots.default;
            {
                var __VLS_thisSlot_8 = __VLS_78.slots.icon;
                var __VLS_83 = {}.IconDelete;
                /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
                // @ts-ignore
                var __VLS_84 = __VLS_asFunctionalComponent(__VLS_83, new __VLS_83({}));
                var __VLS_85 = __VLS_84.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_84), false));
            }
            var __VLS_78;
        }
        var __VLS_42;
    }
    var __VLS_38;
}
var __VLS_21;
var __VLS_87 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_88 = __VLS_asFunctionalComponent(__VLS_87, new __VLS_87(__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.selectableChildDictModalVisible), maskClosable: (false) })));
var __VLS_89 = __VLS_88.apply(void 0, __spreadArray([__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.selectableChildDictModalVisible), maskClosable: (false) })], __VLS_functionalComponentArgsRest(__VLS_88), false));
var __VLS_91;
var __VLS_92;
var __VLS_93;
var __VLS_94 = {
    onOk: (__VLS_ctx.handleAddChildDictsFormSubmit)
};
var __VLS_95 = {
    onCancel: (__VLS_ctx.handleCloseAddChildDictsModal)
};
__VLS_90.slots.default;
{
    var __VLS_thisSlot = __VLS_90.slots.title;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
var __VLS_96 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_97 = __VLS_asFunctionalComponent(__VLS_96, new __VLS_96({
    model: (__VLS_ctx.addChildDictsForm),
    rules: (__VLS_ctx.addChildDictsFormRules),
    ref: "addChildDictsFormRef",
    layout: "vertical",
}));
var __VLS_98 = __VLS_97.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.addChildDictsForm),
        rules: (__VLS_ctx.addChildDictsFormRules),
        ref: "addChildDictsFormRef",
        layout: "vertical",
    }], __VLS_functionalComponentArgsRest(__VLS_97), false));
/** @type {typeof __VLS_ctx.addChildDictsFormRef} */ ;
var __VLS_100 = {};
__VLS_99.slots.default;
var __VLS_102 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_103 = __VLS_asFunctionalComponent(__VLS_102, new __VLS_102({
    field: "children",
    label: "子字典",
}));
var __VLS_104 = __VLS_103.apply(void 0, __spreadArray([{
        field: "children",
        label: "子字典",
    }], __VLS_functionalComponentArgsRest(__VLS_103), false));
__VLS_105.slots.default;
var __VLS_106 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_107 = __VLS_asFunctionalComponent(__VLS_106, new __VLS_106(__assign({ 'onChange': {} }, { placeholder: "请选择子字典", multiple: true, modelValue: (__VLS_ctx.addChildDictsForm.children) })));
var __VLS_108 = __VLS_107.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { placeholder: "请选择子字典", multiple: true, modelValue: (__VLS_ctx.addChildDictsForm.children) })], __VLS_functionalComponentArgsRest(__VLS_107), false));
var __VLS_110;
var __VLS_111;
var __VLS_112;
var __VLS_113 = {
    onChange: (function (val) { return __VLS_ctx.addChildDictsFormRef.validate(); })
};
__VLS_109.slots.default;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.selectableChildDictList)); _i < _a.length; _i++) {
    var child = _a[_i][0];
    var __VLS_114 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_115 = __VLS_asFunctionalComponent(__VLS_114, new __VLS_114({
        key: (child.id),
        value: (child.id),
    }));
    var __VLS_116 = __VLS_115.apply(void 0, __spreadArray([{
            key: (child.id),
            value: (child.id),
        }], __VLS_functionalComponentArgsRest(__VLS_115), false));
    __VLS_117.slots.default;
    (child.name);
    var __VLS_117;
}
var __VLS_109;
var __VLS_105;
var __VLS_118 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_119 = __VLS_asFunctionalComponent(__VLS_118, new __VLS_118({
    field: "relatedDictDataIds",
    label: "关联字典数据",
}));
var __VLS_120 = __VLS_119.apply(void 0, __spreadArray([{
        field: "relatedDictDataIds",
        label: "关联字典数据",
    }], __VLS_functionalComponentArgsRest(__VLS_119), false));
__VLS_121.slots.default;
var __VLS_122 = {}.ASelect;
/** @type {[typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, typeof __VLS_components.ASelect, typeof __VLS_components.aSelect, ]} */ ;
// @ts-ignore
var __VLS_123 = __VLS_asFunctionalComponent(__VLS_122, new __VLS_122(__assign({ 'onChange': {} }, { placeholder: "请选择关联的字典数据", multiple: true, modelValue: (__VLS_ctx.addChildDictsForm.relatedDictDataIds) })));
var __VLS_124 = __VLS_123.apply(void 0, __spreadArray([__assign({ 'onChange': {} }, { placeholder: "请选择关联的字典数据", multiple: true, modelValue: (__VLS_ctx.addChildDictsForm.relatedDictDataIds) })], __VLS_functionalComponentArgsRest(__VLS_123), false));
var __VLS_126;
var __VLS_127;
var __VLS_128;
var __VLS_129 = {
    onChange: (function (val) { return __VLS_ctx.addChildDictsFormRef.validate(); })
};
__VLS_125.slots.default;
for (var _b = 0, _c = __VLS_getVForSourceType((__VLS_ctx.relatableDictDataList)); _b < _c.length; _b++) {
    var data = _c[_b][0];
    var __VLS_130 = {}.AOption;
    /** @type {[typeof __VLS_components.AOption, typeof __VLS_components.aOption, typeof __VLS_components.AOption, typeof __VLS_components.aOption, ]} */ ;
    // @ts-ignore
    var __VLS_131 = __VLS_asFunctionalComponent(__VLS_130, new __VLS_130({
        key: (data.id),
        value: (data.id),
    }));
    var __VLS_132 = __VLS_131.apply(void 0, __spreadArray([{
            key: (data.id),
            value: (data.id),
        }], __VLS_functionalComponentArgsRest(__VLS_131), false));
    __VLS_133.slots.default;
    (data.label);
    var __VLS_133;
}
var __VLS_125;
var __VLS_121;
var __VLS_99;
var __VLS_90;
/** @type {__VLS_StyleScopedClasses['dict-header']} */ ;
/** @type {__VLS_StyleScopedClasses['left']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['info']} */ ;
/** @type {__VLS_StyleScopedClasses['dict-search']} */ ;
/** @type {__VLS_StyleScopedClasses['dict-list']} */ ;
/** @type {__VLS_StyleScopedClasses['table-column-dictname']} */ ;
// @ts-ignore
var __VLS_101 = __VLS_100;
var __VLS_dollars;
var __VLS_self;
