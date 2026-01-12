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
var loading = ref(true);
var htmlReportRef = ref();
var __VLS_props = withDefaults(defineProps(), {
    message: {},
});
var handleIframeLoad = function () {
    var _a;
    var document = htmlReportRef.value.contentDocument ||
        ((_a = htmlReportRef.value.contentWindow) === null || _a === void 0 ? void 0 : _a.document);
    if (document) {
        loading.value = false;
    }
};
var handleFullscreen = function () {
    htmlReportRef.value.requestFullscreen();
};
var handleDownloadReport = function () {
    var _a;
    var iframeDoc = htmlReportRef.value.contentDocument ||
        ((_a = htmlReportRef.value.contentWindow) === null || _a === void 0 ? void 0 : _a.document);
    if (iframeDoc) {
        var htmlContent = iframeDoc.documentElement.outerHTML;
        var blob = new Blob([htmlContent], { type: "text/html" });
        var url_1 = URL.createObjectURL(blob);
        var a_1 = document.createElement("a");
        a_1.href = url_1;
        a_1.download = iframeDoc.title || "report_".concat(new Date().getTime());
        document.body.appendChild(a_1);
        a_1.click();
        setTimeout(function () {
            document.body.removeChild(a_1);
            URL.revokeObjectURL(url_1);
        }, 0);
    }
};
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    message: {},
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
if (__VLS_ctx.message.type === 'HTML_REPORT') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "html-report-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operation-container" }));
    var __VLS_0 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    var __VLS_4 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4(__assign({ 'onClick': {} }, { type: "text", size: "mini", disabled: (__VLS_ctx.loading) })));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini", disabled: (__VLS_ctx.loading) })], __VLS_functionalComponentArgsRest(__VLS_5), false));
    var __VLS_8 = void 0;
    var __VLS_9 = void 0;
    var __VLS_10 = void 0;
    var __VLS_11 = {
        onClick: (__VLS_ctx.handleFullscreen)
    };
    __VLS_7.slots.default;
    {
        var __VLS_thisSlot = __VLS_7.slots.icon;
        var __VLS_12 = {}.IconFullscreen;
        /** @type {[typeof __VLS_components.IconFullscreen, typeof __VLS_components.iconFullscreen, ]} */ ;
        // @ts-ignore
        var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({}));
        var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_13), false));
    }
    {
        var __VLS_thisSlot = __VLS_7.slots.default;
    }
    var __VLS_7;
    var __VLS_16 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign({ 'onClick': {} }, { type: "text", size: "mini", disabled: (__VLS_ctx.loading) })));
    var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini", disabled: (__VLS_ctx.loading) })], __VLS_functionalComponentArgsRest(__VLS_17), false));
    var __VLS_20 = void 0;
    var __VLS_21 = void 0;
    var __VLS_22 = void 0;
    var __VLS_23 = {
        onClick: (__VLS_ctx.handleDownloadReport)
    };
    __VLS_19.slots.default;
    {
        var __VLS_thisSlot = __VLS_19.slots.icon;
        var __VLS_24 = {}.IconDownload;
        /** @type {[typeof __VLS_components.IconDownload, typeof __VLS_components.iconDownload, ]} */ ;
        // @ts-ignore
        var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({}));
        var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_25), false));
    }
    {
        var __VLS_thisSlot = __VLS_19.slots.default;
    }
    var __VLS_19;
    var __VLS_3;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "content" }));
    if (__VLS_ctx.loading) {
        var __VLS_28 = {}.ASkeleton;
        /** @type {[typeof __VLS_components.ASkeleton, typeof __VLS_components.aSkeleton, typeof __VLS_components.ASkeleton, typeof __VLS_components.aSkeleton, ]} */ ;
        // @ts-ignore
        var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
            animation: true,
        }));
        var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
                animation: true,
            }], __VLS_functionalComponentArgsRest(__VLS_29), false));
        __VLS_31.slots.default;
        var __VLS_32 = {}.ASkeletonLine;
        /** @type {[typeof __VLS_components.ASkeletonLine, typeof __VLS_components.aSkeletonLine, ]} */ ;
        // @ts-ignore
        var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({
            rows: (16),
        }));
        var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([{
                rows: (16),
            }], __VLS_functionalComponentArgsRest(__VLS_33), false));
        var __VLS_31;
    }
    __VLS_asFunctionalElement(__VLS_intrinsicElements.iframe)(__assign({ onLoad: (__VLS_ctx.handleIframeLoad) }, { ref: "htmlReportRef", srcdoc: (__VLS_ctx.message.content), frameborder: "0", width: "100%", height: "100%" }));
    __VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (!__VLS_ctx.loading) }), null, null);
    /** @type {typeof __VLS_ctx.htmlReportRef} */ ;
}
/** @type {__VLS_StyleScopedClasses['html-report-container']} */ ;
/** @type {__VLS_StyleScopedClasses['operation-container']} */ ;
/** @type {__VLS_StyleScopedClasses['content']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            loading: loading,
            htmlReportRef: htmlReportRef,
            handleIframeLoad: handleIframeLoad,
            handleFullscreen: handleFullscreen,
            handleDownloadReport: handleDownloadReport,
        };
    },
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
