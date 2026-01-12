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
import VerifySlide from './Verify/VerifySlide.vue';
import VerifyPoints from './Verify/VerifyPoints.vue';
import { computed, ref, watch, toRefs, watchEffect } from 'vue';
export default (await import('vue')).defineComponent({
    name: 'Vue2Verify',
    components: {
        VerifySlide: VerifySlide,
        VerifyPoints: VerifyPoints
    },
    props: {
        captchaType: {
            type: String,
            required: true
        },
        figure: {
            type: Number
        },
        arith: {
            type: Number
        },
        mode: {
            type: String,
            default: 'pop'
        },
        vSpace: {
            type: Number
        },
        explain: {
            type: String
        },
        imgSize: {
            type: Object,
            default: function () {
                return {
                    width: '310px',
                    height: '155px'
                };
            }
        },
        blockSize: {
            type: Object
        },
        barSize: {
            type: Object
        },
    },
    setup: function (props) {
        var _a = toRefs(props), captchaType = _a.captchaType, figure = _a.figure, arith = _a.arith, mode = _a.mode, vSpace = _a.vSpace, explain = _a.explain, imgSize = _a.imgSize, blockSize = _a.blockSize, barSize = _a.barSize;
        var clickShow = ref(false);
        var verifyType = ref(undefined);
        var componentType = ref(undefined);
        var instance = ref({});
        var showBox = computed(function () {
            if (mode.value == 'pop') {
                return clickShow.value;
            }
            else {
                return true;
            }
        });
        /**
         * refresh
         * @description 刷新
         * */
        var refresh = function () {
            // console.log(instance.value);
            if (instance.value.refresh) {
                instance.value.refresh();
            }
        };
        var closeBox = function () {
            clickShow.value = false;
            refresh();
        };
        var show = function () {
            if (mode.value == "pop") {
                clickShow.value = true;
            }
        };
        watchEffect(function () {
            switch (captchaType.value) {
                case 'blockPuzzle':
                    verifyType.value = '2';
                    componentType.value = 'VerifySlide';
                    break;
                case 'clickWord':
                    verifyType.value = '';
                    componentType.value = 'VerifyPoints';
                    break;
            }
        });
        return {
            clickShow: clickShow,
            verifyType: verifyType,
            componentType: componentType,
            instance: instance,
            showBox: showBox,
            closeBox: closeBox,
            show: show
        };
    },
});
var __VLS_ctx = {};
var __VLS_componentsOption = {
    VerifySlide: VerifySlide,
    VerifyPoints: VerifyPoints
};
var __VLS_components;
var __VLS_directives;
// CSS variable injection 
// CSS variable injection end 
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: (__VLS_ctx.mode == 'pop' ? 'mask' : '') }));
__VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (__VLS_ctx.showBox) }), null, null);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: (__VLS_ctx.mode == 'pop' ? 'verifybox' : '') }, { style: ({ 'max-width': parseInt(__VLS_ctx.imgSize.width) + 30 + 'px' }) }));
if (__VLS_ctx.mode == 'pop') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verifybox-top" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ onClick: (__VLS_ctx.closeBox) }, { class: "verifybox-close" }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.i, __VLS_intrinsicElements.i)(__assign({ class: "iconfont icon-close" }));
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verifybox-bottom" }, { style: ({ padding: __VLS_ctx.mode == 'pop' ? '15px' : '0' }) }));
if (__VLS_ctx.componentType) {
    var __VLS_0 = ((__VLS_ctx.componentType));
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
        captchaType: (__VLS_ctx.captchaType),
        type: (__VLS_ctx.verifyType),
        figure: (__VLS_ctx.figure),
        arith: (__VLS_ctx.arith),
        mode: (__VLS_ctx.mode),
        vSpace: (__VLS_ctx.vSpace),
        explain: (__VLS_ctx.explain),
        imgSize: (__VLS_ctx.imgSize),
        blockSize: (__VLS_ctx.blockSize),
        barSize: (__VLS_ctx.barSize),
        ref: "instance",
    }));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
            captchaType: (__VLS_ctx.captchaType),
            type: (__VLS_ctx.verifyType),
            figure: (__VLS_ctx.figure),
            arith: (__VLS_ctx.arith),
            mode: (__VLS_ctx.mode),
            vSpace: (__VLS_ctx.vSpace),
            explain: (__VLS_ctx.explain),
            imgSize: (__VLS_ctx.imgSize),
            blockSize: (__VLS_ctx.blockSize),
            barSize: (__VLS_ctx.barSize),
            ref: "instance",
        }], __VLS_functionalComponentArgsRest(__VLS_1), false));
    /** @type {typeof __VLS_ctx.instance} */ ;
    var __VLS_4 = {};
    var __VLS_3;
}
/** @type {__VLS_StyleScopedClasses['verifybox-top']} */ ;
/** @type {__VLS_StyleScopedClasses['verifybox-close']} */ ;
/** @type {__VLS_StyleScopedClasses['iconfont']} */ ;
/** @type {__VLS_StyleScopedClasses['icon-close']} */ ;
/** @type {__VLS_StyleScopedClasses['verifybox-bottom']} */ ;
// @ts-ignore
var __VLS_5 = __VLS_4;
var __VLS_dollars;
var __VLS_self;
