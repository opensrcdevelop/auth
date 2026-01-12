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
import Menu from "./components/Menu.vue";
import { onMounted, reactive } from "vue";
import { getCurrentUser } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { logoutSubmit } from "@/api/logout";
import { Modal, Notification } from "@arco-design/web-vue";
import router from "@/router";
import { AUTH_TOKENS } from "@/util/constants";
/** 当前用户信息 */
var currentUser = reactive({
    id: "",
    username: "",
    consoleAccess: undefined,
});
/**
 * 获取当前用户信息
 */
var handleGetCurrentUser = function () {
    getCurrentUser()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            currentUser.id = data.id;
            currentUser.username = data.username;
            currentUser.consoleAccess = data.consoleAccess;
            checkConsoleAccess();
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取当前用户信息");
    });
};
/**
 * 退出登录
 */
var handleLogout = function () {
    Modal.warning({
        title: "确定退出登录？",
        content: "",
        hideCancel: false,
        okButtonProps: {
            status: "warning",
        },
        onOk: function () {
            logoutSubmit()
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("退出成功");
                    localStorage.removeItem(AUTH_TOKENS);
                    // 跳转到登录页
                    router.push({
                        path: "/oauth2/redirect",
                    });
                });
            })
                .catch(function (err) {
                handleApiError(err, "退出登录");
            });
        },
    });
};
/**
 * 跳转至个人中心
 */
var handleToUserCenter = function () {
    router.push({
        path: "/user/home",
    });
};
/**
 * 检查控制台访问权限
 */
var checkConsoleAccess = function () {
    if (!currentUser.consoleAccess) {
        router.push({
            path: "/403",
        });
    }
};
onMounted(function () {
    handleGetCurrentUser();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
/** @type {__VLS_StyleScopedClasses['operation-item']} */ ;
// CSS variable injection 
// CSS variable injection end 
var __VLS_0 = {}.ALayoutSider;
/** @type {[typeof __VLS_components.ALayoutSider, typeof __VLS_components.aLayoutSider, typeof __VLS_components.ALayoutSider, typeof __VLS_components.aLayoutSider, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ class: "layout-sider" })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ class: "layout-sider" })], __VLS_functionalComponentArgsRest(__VLS_1), false));
var __VLS_4 = {};
__VLS_3.slots.default;
/** @type {[typeof Menu, typeof Menu, ]} */ ;
// @ts-ignore
var __VLS_5 = __VLS_asFunctionalComponent(Menu, new Menu(__assign({ class: "menu" })));
var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign({ class: "menu" })], __VLS_functionalComponentArgsRest(__VLS_5), false));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "action-container" }));
var __VLS_8 = {}.AAvatar;
/** @type {[typeof __VLS_components.AAvatar, typeof __VLS_components.aAvatar, typeof __VLS_components.AAvatar, typeof __VLS_components.aAvatar, ]} */ ;
// @ts-ignore
var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign({ style: ({ backgroundColor: '#396aff' }) }, { class: "avatar" })));
var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign({ style: ({ backgroundColor: '#396aff' }) }, { class: "avatar" })], __VLS_functionalComponentArgsRest(__VLS_9), false));
__VLS_11.slots.default;
(__VLS_ctx.currentUser.username);
var __VLS_11;
var __VLS_12 = {}.ATrigger;
/** @type {[typeof __VLS_components.ATrigger, typeof __VLS_components.aTrigger, typeof __VLS_components.ATrigger, typeof __VLS_components.aTrigger, ]} */ ;
// @ts-ignore
var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({
    position: "right",
    popupTranslate: ([10, -10]),
}));
var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{
        position: "right",
        popupTranslate: ([10, -10]),
    }], __VLS_functionalComponentArgsRest(__VLS_13), false));
__VLS_15.slots.default;
var __VLS_16 = {}.AButton;
/** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
// @ts-ignore
var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign({ type: "text", size: "large" }, { class: "trigger-button" })));
var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign({ type: "text", size: "large" }, { class: "trigger-button" })], __VLS_functionalComponentArgsRest(__VLS_17), false));
__VLS_19.slots.default;
{
    var __VLS_thisSlot = __VLS_19.slots.icon;
    var __VLS_20 = {}.IconMore;
    /** @type {[typeof __VLS_components.IconMore, typeof __VLS_components.iconMore, ]} */ ;
    // @ts-ignore
    var __VLS_21 = __VLS_asFunctionalComponent(__VLS_20, new __VLS_20({}));
    var __VLS_22 = __VLS_21.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_21), false));
}
var __VLS_19;
{
    var __VLS_thisSlot = __VLS_15.slots.content;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.handleToUserCenter) }, { class: "operation-item" }));
    var __VLS_24 = {}.IconUser;
    /** @type {[typeof __VLS_components.IconUser, typeof __VLS_components.iconUser, ]} */ ;
    // @ts-ignore
    var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({}));
    var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_25), false));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.handleLogout) }, { class: "operation-item" }));
    var __VLS_28 = {}.IconPoweroff;
    /** @type {[typeof __VLS_components.IconPoweroff, typeof __VLS_components.iconPoweroff, ]} */ ;
    // @ts-ignore
    var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({}));
    var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_29), false));
}
var __VLS_15;
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['layout-sider']} */ ;
/** @type {__VLS_StyleScopedClasses['menu']} */ ;
/** @type {__VLS_StyleScopedClasses['action-container']} */ ;
/** @type {__VLS_StyleScopedClasses['avatar']} */ ;
/** @type {__VLS_StyleScopedClasses['trigger-button']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-container']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-item']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-item']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            Menu: Menu,
            currentUser: currentUser,
            handleLogout: handleLogout,
            handleToUserCenter: handleToUserCenter,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
