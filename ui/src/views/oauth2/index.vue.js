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
import { base64Str, generateCodeChallenge, generateRandomString, getConsoleUrl, getOAuthIssuer, getQueryString, handleApiError, handleApiSuccess, } from "@/util/tool";
import { getToken } from "@/api/login";
import router from "@/router";
import { ref } from "vue";
import { logoutSubmit } from "@/api/logout";
import { AUTH_TOKENS, AUTHORIZAION_CODE, CODE, CODE_VERIFIER, REDIRECT_PATH, REDIRECT_QUERY, STATE, } from "@/util/constants";
import { getCurrentUser } from "@/api/user";
// 获取地址栏授权码
var code = getQueryString(CODE);
var loading = ref(true);
var hasError = ref(false);
var errorText = ref("");
if (code) {
    // 校验 state，防止 CSRF
    var state = localStorage.getItem(STATE);
    var urlState = getQueryString(STATE);
    if (urlState !== state) {
        hasError.value = true;
        errorText.value = "state 不匹配";
    }
    else {
        // 获取 token
        getToken({
            grant_type: AUTHORIZAION_CODE,
            client_id: import.meta.env.VITE_OAUTH_CLIENT_ID,
            redirect_uri: "".concat(getConsoleUrl()).concat(import.meta.env.VITE_UI_BASE_PATH || "", "/oauth2/redirect"),
            code: code,
            code_verifier: localStorage.getItem(CODE_VERIFIER),
            state: state,
        })
            .then(function (res) {
            localStorage.setItem(AUTH_TOKENS, base64Str(JSON.stringify(res)));
            localStorage.removeItem(STATE);
            localStorage.removeItem(CODE_VERIFIER);
            var redirectPath = localStorage.getItem(REDIRECT_PATH);
            var redirectQuery = localStorage.getItem(REDIRECT_QUERY);
            // 跳转到目标路径
            if (localStorage.getItem(REDIRECT_PATH)) {
                if (redirectQuery) {
                    router.push({
                        path: redirectPath,
                        query: JSON.parse(redirectQuery),
                    });
                }
                else {
                    router.push({ path: redirectPath });
                }
                localStorage.removeItem(REDIRECT_PATH);
                localStorage.removeItem(REDIRECT_QUERY);
            }
            else {
                // 获取用户是否有控制台权限
                getCurrentUser().then(function (result) {
                    handleApiSuccess(result, function (data) {
                        if (data.consoleAccess) {
                            // 跳转到首页
                            router.push({ path: "/" });
                        }
                        else {
                            // 跳转到用户中心
                            router.push({ path: "/user/home" });
                        }
                    });
                });
            }
        })
            .catch(function (err) {
            hasError.value = true;
            errorText.value = err.data.message || err.statusText;
        });
    }
}
else {
    // 生成 state
    var state = generateRandomString(32);
    // 生成 CodeVerifier
    var codeVerifier = generateRandomString(32);
    // 生成 CodeChallenge
    var codeChallenge = generateCodeChallenge(codeVerifier);
    // 缓存 state 和 codeVerifier
    localStorage.setItem(STATE, state);
    localStorage.setItem(CODE_VERIFIER, codeVerifier);
    // 获取授权码
    window.location.href = "".concat(getOAuthIssuer()).concat(import.meta.env.VITE_API_BASE_URI, "/oauth2/authorize?client_id=").concat(import.meta.env.VITE_OAUTH_CLIENT_ID, "&response_type=code&redirect_uri=").concat(getConsoleUrl()).concat(import.meta.env.VITE_UI_BASE_PATH || "", "/oauth2/redirect&code_challenge=").concat(codeChallenge, "&code_challenge_method=S256&state=").concat(state);
}
loading.value = false;
var handleRetry = function () {
    logoutSubmit()
        .then(function (result) {
        handleApiSuccess(result, function () {
            window.location.href = "".concat(getConsoleUrl()).concat(import.meta.env.VITE_UI_BASE_PATH || "");
        });
    })
        .catch(function (err) {
        handleApiError(err, "退出登录");
    });
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "result-container" }));
var __VLS_0 = {}.ASpin;
/** @type {[typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, typeof __VLS_components.ASpin, typeof __VLS_components.aSpin, ]} */ ;
// @ts-ignore
var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0(__assign({ style: {} }, { loading: (__VLS_ctx.loading), tip: "Loading..." })));
var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([__assign({ style: {} }, { loading: (__VLS_ctx.loading), tip: "Loading..." })], __VLS_functionalComponentArgsRest(__VLS_1), false));
__VLS_3.slots.default;
if (__VLS_ctx.hasError) {
    var __VLS_4 = {}.AResult;
    /** @type {[typeof __VLS_components.AResult, typeof __VLS_components.aResult, typeof __VLS_components.AResult, typeof __VLS_components.aResult, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4({
        status: "warning",
    }));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([{
            status: "warning",
        }], __VLS_functionalComponentArgsRest(__VLS_5), false));
    __VLS_7.slots.default;
    {
        var __VLS_thisSlot = __VLS_7.slots.title;
        __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
    }
    {
        var __VLS_thisSlot = __VLS_7.slots.subtitle;
        (__VLS_ctx.errorText);
    }
    {
        var __VLS_thisSlot = __VLS_7.slots.extra;
        var __VLS_8 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_9 = __VLS_asFunctionalComponent(__VLS_8, new __VLS_8(__assign({ 'onClick': {} }, { type: "text" })));
        var __VLS_10 = __VLS_9.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text" })], __VLS_functionalComponentArgsRest(__VLS_9), false));
        var __VLS_12 = void 0;
        var __VLS_13 = void 0;
        var __VLS_14 = void 0;
        var __VLS_15 = {
            onClick: (__VLS_ctx.handleRetry)
        };
        __VLS_11.slots.default;
        {
            var __VLS_thisSlot_1 = __VLS_11.slots.icon;
            var __VLS_16 = {}.IconRefresh;
            /** @type {[typeof __VLS_components.IconRefresh, typeof __VLS_components.iconRefresh, ]} */ ;
            // @ts-ignore
            var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16({}));
            var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_17), false));
        }
        var __VLS_11;
    }
    var __VLS_7;
}
var __VLS_3;
/** @type {__VLS_StyleScopedClasses['result-container']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            loading: loading,
            hasError: hasError,
            errorText: errorText,
            handleRetry: handleRetry,
        };
    },
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
});
; /* PartiallyEnd: #4569/main.vue */
