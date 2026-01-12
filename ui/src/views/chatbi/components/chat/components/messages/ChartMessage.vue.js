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
import * as echarts from "echarts";
import { onUnmounted } from "vue";
var __VLS_props = withDefaults(defineProps(), {
    message: {},
});
var chartRefs = new Map();
var initChart = function (el, option) {
    if (!el || !option)
        return;
    var existingChart = chartRefs.get(el);
    if (existingChart) {
        var existingOption = existingChart.getOption();
        var isOptionChanged = JSON.stringify(existingOption) !== JSON.stringify(option);
        if (!isOptionChanged) {
            return;
        }
        existingChart.setOption(option, true);
        return;
    }
    setTimeout(function () {
        var chart = echarts.init(el);
        chart.setOption(option);
        chart.resize();
        var resizeObserver = new ResizeObserver(function () {
            chart.resize();
        });
        resizeObserver.observe(el);
        chartRefs.set(el, chart);
        chartRefs.set(el + "_observer", resizeObserver);
    }, 0);
};
onUnmounted(function () {
    chartRefs.forEach(function (value, key) {
        if (typeof key === "string" && key.endsWith("_observer")) {
            value.disconnect();
        }
        else {
            value.dispose();
        }
    });
    chartRefs.clear();
});
debugger; /* PartiallyEnd: #3632/scriptSetup.vue */
var __VLS_withDefaultsArg = (function (t) { return t; })({
    message: {},
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
if (__VLS_ctx.message.type === 'CHART') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "echarts-container" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "title" }));
    (__VLS_ctx.message.content.title.text);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "description" }));
    (__VLS_ctx.message.content.title.description);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "chart" }, { ref: (function (el) { var _a; return __VLS_ctx.initChart(el, (_a = __VLS_ctx.message.content) === null || _a === void 0 ? void 0 : _a.option); }) }));
}
/** @type {__VLS_StyleScopedClasses['echarts-container']} */ ;
/** @type {__VLS_StyleScopedClasses['title']} */ ;
/** @type {__VLS_StyleScopedClasses['description']} */ ;
/** @type {__VLS_StyleScopedClasses['chart']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            initChart: initChart,
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
