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
import { getEnabledIdentitySource } from "@/api/identitySource";
import router from "@/router";
import { AUTH_FAILURE, AUTH_SUCCESS, USER_LOCKED } from "@/util/constants";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { computed, onMounted, reactive, ref } from "vue";
// 身份源列表
var identitySourceList = reactive([]);
// 是否启用第三方登录
var federationLoginEnabled = computed(function () {
    return identitySourceList.length > 0;
});
var loading = ref(false);
onMounted(function () {
    handleGetEnabledIdentitySource();
    window.addEventListener("message", handleAuthWindowResponse);
});
/**
 * 打开第三方认证窗口
 */
var authWindow;
var handleOpenAuthWindow = function (authorizationUri) {
    authWindow = window.open(authorizationUri, "_blank", "width=600,height=600");
};
/**
 * 处理第三方认证窗口响应
 */
var handleAuthWindowResponse = function (event) {
    if (event.data === AUTH_SUCCESS) {
        Notification.success("第三方账号登录成功");
        authWindow.close();
        toTarget();
    }
    if (event.data === AUTH_FAILURE) {
        Notification.error("第三方账号登录失败");
        authWindow.close();
    }
    if (event.data === USER_LOCKED) {
        Notification.error("账号已被禁用，无法通过第三方账号登录");
        authWindow.close();
    }
};
/**
 * 跳转至目标路径
 */
var toTarget = function () {
    var target = getQueryString("target");
    if (target) {
        window.location.href = target;
    }
    else {
        router.push({
            path: "/",
        });
    }
};
var handleGetEnabledIdentitySource = function () {
    loading.value = true;
    getEnabledIdentitySource()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            identitySourceList.length = 0;
            identitySourceList.push.apply(identitySourceList, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取启用的身份源");
    })
        .finally(function () {
        loading.value = false;
    });
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
/** @type {__VLS_StyleScopedClasses['federation-login-title']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-title']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-title']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-title']} */ ;
// CSS variable injection 
// CSS variable injection end 
if (__VLS_ctx.loading) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "loading-container" }));
    var __VLS_0 = {}.ASkeleton;
    /** @type {[typeof __VLS_components.ASkeleton, typeof __VLS_components.aSkeleton, typeof __VLS_components.ASkeleton, typeof __VLS_components.aSkeleton, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
        animation: true,
    }));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
            animation: true,
        }], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    var __VLS_4 = {}.ASkeletonLine;
    /** @type {[typeof __VLS_components.ASkeletonLine, typeof __VLS_components.aSkeletonLine, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
        rows: (3),
    }));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([{
            rows: (3),
        }], __VLS_functionalComponentArgsRest(__VLS_5), false));
    var __VLS_3;
}
if (__VLS_ctx.federationLoginEnabled) {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "federation-login-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "federation-login-title" }));
    var _loop_1 = function (item) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "federation-login-button-container" }));
        var __VLS_8 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign(__assign({ 'onClick': {} }, { key: (item.code) }), { class: "federation-login-button" })));
        var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign(__assign({ 'onClick': {} }, { key: (item.code) }), { class: "federation-login-button" })], __VLS_functionalComponentArgsRest(__VLS_9), false));
        var __VLS_12 = void 0;
        var __VLS_13 = void 0;
        var __VLS_14 = void 0;
        var __VLS_15 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.federationLoginEnabled))
                    return;
                __VLS_ctx.handleOpenAuthWindow(item.authorizationUri);
            }
        };
        __VLS_11.slots.default;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "federation-login-button-content" }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.img)(__assign({ class: "logo" }, { src: (item.logo), draggable: (false) }));
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "text" }));
        (item.name);
    };
    var __VLS_11;
    for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.identitySourceList)); _i < _a.length; _i++) {
        var item = _a[_i][0];
        _loop_1(item);
    }
}
/** @type {__VLS_StyleScopedClasses['loading-container']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-container']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-title']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-button-container']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-button']} */ ;
/** @type {__VLS_StyleScopedClasses['federation-login-button-content']} */ ;
/** @type {__VLS_StyleScopedClasses['logo']} */ ;
/** @type {__VLS_StyleScopedClasses['text']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            identitySourceList: identitySourceList,
            federationLoginEnabled: federationLoginEnabled,
            loading: loading,
            handleOpenAuthWindow: handleOpenAuthWindow,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
