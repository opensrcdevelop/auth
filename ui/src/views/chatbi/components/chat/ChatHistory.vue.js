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
import { deleteChatHistory, getUserChatHistory, updateChatHistoryTitle, } from "@/api/chatbi";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { onMounted, onUnmounted, reactive, ref } from "vue";
var emits = defineEmits();
var isCollapsed = ref(false);
var isOverlay = ref(false);
var BREAKPOINT = 768;
var handleResize = function () {
    isCollapsed.value = window.innerWidth < BREAKPOINT;
    isOverlay.value = window.innerWidth < BREAKPOINT;
};
onMounted(function () {
    handleResize();
    window.addEventListener("resize", handleResize);
});
onUnmounted(function () {
    window.removeEventListener("resize", handleResize);
});
var toggleCollapse = function () {
    isCollapsed.value = !isCollapsed.value;
    if (window.innerWidth < BREAKPOINT) {
        isOverlay.value = !isCollapsed.value;
    }
};
/**
 * 对话历史记录
 */
var searchKeyword = ref("");
var chatHistoryList = reactive([]);
var handleGetChatHistoryList = function () {
    getUserChatHistory(searchKeyword.value)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            chatHistoryList.length = 0;
            chatHistoryList.push.apply(chatHistoryList, data);
            if (data.length === 0) {
                handleAddNewChat();
            }
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取对话历史记录");
    });
};
/** 当前对话ID */
var activeChatId = ref("");
/**
 * 切换对话
 */
var handleSwitchChat = function (chatHistory) {
    activeChatId.value = chatHistory.id;
    emits("switchChat", chatHistory.id);
    emits("updateDataSourceId", chatHistory.dataSourceId);
};
/**
 * 开启新对话
 */
var handleAddNewChat = function () {
    activeChatId.value = "";
    emits("addNewChat");
};
/**
 * 删除对话历史记录
 */
var handleDeleteChatHistory = function (chatHistory) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u5BF9\u8BDD\u300C".concat(chatHistory.title, "\uFF08").concat(chatHistory.start, "\uFF09\u300D\u5417\uFF1F"),
        content: "此操作将删除该对话关联所有历史记录，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteChatHistory(chatHistory.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetChatHistoryList();
                    if (chatHistory.id === activeChatId.value) {
                        handleAddNewChat();
                    }
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除对话历史记录");
            });
        },
    });
};
/**
 * 重命名对话
 */
var handleRenameChatHistoryTitle = function (chatHistory) {
    updateTitleForm.id = chatHistory.id;
    updateTitleForm.title = chatHistory.title;
    editTitleModalVisible.value = true;
};
/**
 * 修改对话标题对话框
 */
var editTitleModalVisible = ref(false);
var updateTitleFormRef = ref();
var updateTitleForm = reactive({
    id: "",
    title: "",
});
var updateTitleFormRules = {
    title: [
        {
            validator: function (value, cb) {
                if (!value || value.trim().length === 0) {
                    cb("对话标题不能为空");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
var handleUpdateTitleFormSubmit = function () {
    updateTitleFormRef.value.validate(function (errors) {
        if (!errors) {
            updateChatHistoryTitle(updateTitleForm)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("修改成功");
                    editTitleModalVisible.value = false;
                    handleGetChatHistoryList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "修改对话标题");
            });
        }
    });
};
var init = function (chatId) {
    if (chatId === void 0) { chatId = ""; }
    activeChatId.value = chatId;
    handleGetChatHistoryList();
};
var __VLS_exposed = {
    init: init,
};
defineExpose(__VLS_exposed);
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
/** @type {__VLS_StyleScopedClasses['button-container']} */ ;
/** @type {__VLS_StyleScopedClasses['active']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-item']} */ ;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chat-history-container" }, { class: ({
        collapsed: __VLS_ctx.isCollapsed,
        overlay: __VLS_ctx.isOverlay && !__VLS_ctx.isCollapsed,
    }) }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "button-container" }));
var __VLS_0 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign(__assign({ 'onClick': {} }, { type: "text", shape: "circle" }), { style: {} })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text", shape: "circle" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4;
var __VLS_5;
var __VLS_6;
var __VLS_7 = {
    onClick: (__VLS_ctx.toggleCollapse)
};
__VLS_3.slots.default;
{
    var __VLS_thisSlot = __VLS_3.slots.icon;
    if (!__VLS_ctx.isCollapsed) {
        var __VLS_8 = {}.IconMenuFold;
        /** @type {[typeof __VLS_components.IconMenuFold, typeof __VLS_components.iconMenuFold, ]} */ ;
        // @ts-ignore
        var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8({}));
        var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_9), false));
    }
    else {
        var __VLS_12 = {}.IconMenuUnfold;
        /** @type {[typeof __VLS_components.IconMenuUnfold, typeof __VLS_components.iconMenuUnfold, ]} */ ;
        // @ts-ignore
        var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({}));
        var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_13), false));
    }
}
var __VLS_3;
var __VLS_16 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign(__assign({ 'onClick': {} }, { type: "text", shape: "circle" }), { style: {} })));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { type: "text", shape: "circle" }), { style: {} })], __VLS_functionalComponentArgsRest(__VLS_17), false));
var __VLS_20;
var __VLS_21;
var __VLS_22;
var __VLS_23 = {
    onClick: (__VLS_ctx.handleAddNewChat)
};
__VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (__VLS_ctx.isCollapsed) }), null, null);
__VLS_19.slots.default;
{
    var __VLS_thisSlot = __VLS_19.slots.icon;
    var __VLS_24 = {}.IconPlusCircle;
    /** @type {[typeof __VLS_components.IconPlusCircle, typeof __VLS_components.iconPlusCircle, ]} */ ;
    // @ts-ignore
    var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({}));
    var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_25), false));
}
var __VLS_19;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.handleAddNewChat) }, { class: "add-chat-container" }));
__VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (!__VLS_ctx.isCollapsed) }), null, null);
var __VLS_28 = {}.IconPlusCircle;
/** @type {[typeof __VLS_components.IconPlusCircle, typeof __VLS_components.iconPlusCircle, ]} */ ;
// @ts-ignore
var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({}));
var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_29), false));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "text" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "search-container" }));
__VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (!__VLS_ctx.isCollapsed) }), null, null);
var __VLS_32 = {}.AInputSearch;
/** @type {[typeof __VLS_components.AInputSearch, typeof __VLS_components.aInputSearch, ]} */ ;
// @ts-ignore
var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32(__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '240px', backgroundColor: '#fff' }) }), { placeholder: "搜索历史对话", allowClear: true, modelValue: (__VLS_ctx.searchKeyword) })));
var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([__assign(__assign(__assign(__assign({ 'onSearch': {} }, { 'onKeyup': {} }), { 'onClear': {} }), { style: ({ width: '240px', backgroundColor: '#fff' }) }), { placeholder: "搜索历史对话", allowClear: true, modelValue: (__VLS_ctx.searchKeyword) })], __VLS_functionalComponentArgsRest(__VLS_33), false));
var __VLS_36;
var __VLS_37;
var __VLS_38;
var __VLS_39 = {
    onSearch: (__VLS_ctx.handleGetChatHistoryList)
};
var __VLS_40 = {
    onKeyup: (__VLS_ctx.handleGetChatHistoryList)
};
var __VLS_41 = {
    onClear: (__VLS_ctx.handleGetChatHistoryList)
};
var __VLS_35;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chat-history-list" }));
var _loop_1 = function (item) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign(__assign({ onClick: function () {
            var _a = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                _a[_i] = arguments[_i];
            }
            var $event = _a[0];
            __VLS_ctx.handleSwitchChat(item);
        } }, { class: "chat-history-item" }), { class: ({
            active: item.id === __VLS_ctx.activeChatId,
        }) }), { key: (item.id) }));
    __VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (!__VLS_ctx.isCollapsed) }), null, null);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "content" }));
    var __VLS_42 = {}.ATooltip;
    /** @type {[typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, ]} */ ;
    // @ts-ignore
    var __VLS_43 = __VLS_asFunctionalComponent(__VLS_42, new __VLS_42({
        content: (item.title),
    }));
    var __VLS_44 = __VLS_43.apply(void 0, __spreadArray([{
            content: (item.title),
        }], __VLS_functionalComponentArgsRest(__VLS_43), false));
    __VLS_45.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }, { class: ({
            active: item.id === __VLS_ctx.activeChatId,
        }) }));
    (item.title);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "time" }));
    (item.start);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation" }));
    var __VLS_46 = {}.ATrigger;
    /** @type {[typeof __VLS_components.ATrigger, typeof __VLS_components.aTrigger, typeof __VLS_components.ATrigger, typeof __VLS_components.aTrigger, ]} */ ;
    // @ts-ignore
    var __VLS_47 = __VLS_asFunctionalComponent(__VLS_46, new __VLS_46({
        trigger: "hover",
        position: "bottom",
        popupTranslate: ([-50, 4]),
    }));
    var __VLS_48 = __VLS_47.apply(void 0, __spreadArray([{
            trigger: "hover",
            position: "bottom",
            popupTranslate: ([-50, 4]),
        }], __VLS_functionalComponentArgsRest(__VLS_47), false));
    __VLS_49.slots.default;
    var __VLS_50 = {}.IconMore;
    /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
    // @ts-ignore
    var __VLS_51 = __VLS_asFunctionalComponent(__VLS_50, new __VLS_50(__assign({ style: {} })));
    var __VLS_52 = __VLS_51.apply(void 0, __spreadArray([__assign({ style: {} })], __VLS_functionalComponentArgsRest(__VLS_51), false));
    {
        var __VLS_thisSlot = __VLS_49.slots.content;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation-container" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleRenameChatHistoryTitle(item);
            } }, { class: "operation-item" }));
        var __VLS_54 = {}.IconEdit;
        /** @type {[typeof __VLS_components.IconEdit, typeof __VLS_components.iconEdit, ]} */ ;
        // @ts-ignore
        var __VLS_55 = __VLS_asFunctionalComponent(__VLS_54, new __VLS_54({}));
        var __VLS_56 = __VLS_55.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_55), false));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                __VLS_ctx.handleDeleteChatHistory(item);
            } }, { class: "operation-item" }), { style: {} }));
        var __VLS_58 = {}.IconDelete;
        /** @type {[typeof __VLS_components.IconDelete, typeof __VLS_components.iconDelete, ]} */ ;
        // @ts-ignore
        var __VLS_59 = __VLS_asFunctionalComponent(__VLS_58, new __VLS_58({}));
        var __VLS_60 = __VLS_59.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_59), false));
    }
};
var __VLS_45, __VLS_49;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.chatHistoryList)); _i < _a.length; _i++) {
    var item = _a[_i][0];
    _loop_1(item);
}
var __VLS_62 = {}.AModal;
/** @type {[typeof __VLS_components.AModal, typeof __VLS_components.aModal, typeof __VLS_components.AModal, typeof __VLS_components.aModal, ]} */ ;
// @ts-ignore
var __VLS_63 = __VLS_asFunctionalComponent(__VLS_62, new __VLS_62(__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.editTitleModalVisible) })));
var __VLS_64 = __VLS_63.apply(void 0, __spreadArray([__assign(__assign({ 'onOk': {} }, { 'onCancel': {} }), { visible: (__VLS_ctx.editTitleModalVisible) })], __VLS_functionalComponentArgsRest(__VLS_63), false));
var __VLS_66;
var __VLS_67;
var __VLS_68;
var __VLS_69 = {
    onOk: (__VLS_ctx.handleUpdateTitleFormSubmit)
};
var __VLS_70 = {
    onCancel: (function () { return (__VLS_ctx.editTitleModalVisible = false); })
};
__VLS_65.slots.default;
{
    var __VLS_thisSlot = __VLS_65.slots.title;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
var __VLS_71 = {}.AForm;
/** @type {[typeof __VLS_components.AForm, typeof __VLS_components.aForm, typeof __VLS_components.AForm, typeof __VLS_components.aForm, ]} */ ;
// @ts-ignore
var __VLS_72 = __VLS_asFunctionalComponent(__VLS_71, new __VLS_71({
    model: (__VLS_ctx.updateTitleForm),
    rules: (__VLS_ctx.updateTitleFormRules),
    ref: "updateTitleFormRef",
}));
var __VLS_73 = __VLS_72.apply(void 0, __spreadArray([{
        model: (__VLS_ctx.updateTitleForm),
        rules: (__VLS_ctx.updateTitleFormRules),
        ref: "updateTitleFormRef",
    }], __VLS_functionalComponentArgsRest(__VLS_72), false));
/** @type {typeof __VLS_ctx.updateTitleFormRef} */ ;
var __VLS_75 = {};
__VLS_74.slots.default;
var __VLS_77 = {}.AFormItem;
/** @type {[typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, typeof __VLS_components.AFormItem, typeof __VLS_components.aFormItem, ]} */ ;
// @ts-ignore
var __VLS_78 = __VLS_asFunctionalComponent(__VLS_77, new __VLS_77({
    field: "title",
    hideLabel: true,
}));
var __VLS_79 = __VLS_78.apply(void 0, __spreadArray([{
        field: "title",
        hideLabel: true,
    }], __VLS_functionalComponentArgsRest(__VLS_78), false));
__VLS_80.slots.default;
var __VLS_81 = {}.AInput;
/** @type {[typeof __VLS_components.AInput, typeof __VLS_components.aInput, ]} */ ;
// @ts-ignore
var __VLS_82 = __VLS_asFunctionalComponent(__VLS_81, new __VLS_81({
    modelValue: (__VLS_ctx.updateTitleForm.title),
    placeholder: "请输入对话标题",
}));
var __VLS_83 = __VLS_82.apply(void 0, __spreadArray([{
        modelValue: (__VLS_ctx.updateTitleForm.title),
        placeholder: "请输入对话标题",
    }], __VLS_functionalComponentArgsRest(__VLS_82), false));
var __VLS_80;
var __VLS_74;
var __VLS_65;
/** @type {__VLS_StyleScopedClasses['chat-history-container']} */ ;
/** @type {__VLS_StyleScopedClasses['collapsed']} */ ;
/** @type {__VLS_StyleScopedClasses['overlay']} */ ;
/** @type {__VLS_StyleScopedClasses['button-container']} */ ;
/** @type {__VLS_StyleScopedClasses['add-chat-container']} */ ;
/** @type {__VLS_StyleScopedClasses['text']} */ ;
/** @type {__VLS_StyleScopedClasses['search-container']} */ ;
/** @type {__VLS_StyleScopedClasses['chat-history-list']} */ ;
/** @type {__VLS_StyleScopedClasses['chat-history-item']} */ ;
/** @type {__VLS_StyleScopedClasses['active']} */ ;
/** @type {__VLS_StyleScopedClasses['content']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['active']} */ ;
/** @type {__VLS_StyleScopedClasses['time']} */ ;
/** @type {__VLS_StyleScopedClasses['operation']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-container']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-item']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-item']} */ ;
// @ts-ignore
var __VLS_76 = __VLS_75;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            isCollapsed: isCollapsed,
            isOverlay: isOverlay,
            toggleCollapse: toggleCollapse,
            searchKeyword: searchKeyword,
            chatHistoryList: chatHistoryList,
            handleGetChatHistoryList: handleGetChatHistoryList,
            activeChatId: activeChatId,
            handleSwitchChat: handleSwitchChat,
            handleAddNewChat: handleAddNewChat,
            handleDeleteChatHistory: handleDeleteChatHistory,
            handleRenameChatHistoryTitle: handleRenameChatHistoryTitle,
            editTitleModalVisible: editTitleModalVisible,
            updateTitleFormRef: updateTitleFormRef,
            updateTitleForm: updateTitleForm,
            updateTitleFormRules: updateTitleFormRules,
            handleUpdateTitleFormSubmit: handleUpdateTitleFormSubmit,
        };
    },
    __typeEmits: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return __assign({}, __VLS_exposed);
    },
    __typeEmits: {},
});
; /* PartiallyEnd: #4569/main.vue */
