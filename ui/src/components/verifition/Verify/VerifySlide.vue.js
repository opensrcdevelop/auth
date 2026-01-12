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
import { aesEncrypt } from "../utils/ase";
import { resetSize } from '../utils/util';
import { reqCheck, reqGet } from "../api/index";
import { computed, getCurrentInstance, nextTick, onMounted, reactive, ref, toRefs, watch } from 'vue';
//  "captchaType":"blockPuzzle",
export default (await import('vue')).defineComponent({
    name: 'VerifySlide',
    props: {
        captchaType: {
            type: String,
        },
        type: {
            type: String,
            default: '1'
        },
        //弹出式pop，固定fixed
        mode: {
            type: String,
            default: 'fixed'
        },
        vSpace: {
            type: Number,
            default: 5
        },
        explain: {
            type: String,
            default: '向右滑动完成验证'
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
            type: Object,
            default: function () {
                return {
                    width: '50px',
                    height: '50px'
                };
            }
        },
        barSize: {
            type: Object,
            default: function () {
                return {
                    width: '310px',
                    height: '40px'
                };
            }
        }
    },
    setup: function (props, context) {
        var _a = toRefs(props), mode = _a.mode, captchaType = _a.captchaType, vSpace = _a.vSpace, imgSize = _a.imgSize, barSize = _a.barSize, type = _a.type, blockSize = _a.blockSize, explain = _a.explain;
        var proxy = getCurrentInstance().proxy;
        var secretKey = ref(''), //后端返回的ase加密秘钥
        passFlag = ref(''), //是否通过的标识
        backImgBase = ref(''), //验证码背景图片
        blockBackImgBase = ref(''), //验证滑块的背景图片
        backToken = ref(''), //后端返回的唯一token值
        startMoveTime = ref(''), //移动开始的时间
        endMovetime = ref(''), //移动结束的时间
        tipsBackColor = ref(''), //提示词的背景颜色
        tipWords = ref(''), text = ref(''), finishText = ref(''), setSize = reactive({
            imgHeight: 0,
            imgWidth: 0,
            barHeight: 0,
            barWidth: 0
        }), top = ref(0), left = ref(0), moveBlockLeft = ref(undefined), leftBarWidth = ref(undefined), 
        // 移动中样式
        moveBlockBackgroundColor = ref(undefined), leftBarBorderColor = ref('#ddd'), iconColor = ref(undefined), iconClass = ref('icon-right'), status = ref(false), //鼠标状态
        isEnd = ref(false), //是够验证完成
        showRefresh = ref(true), transitionLeft = ref(''), transitionWidth = ref(''), startLeft = ref(0);
        var barArea = computed(function () {
            return proxy.$el.querySelector('.verify-bar-area');
        });
        function init() {
            text.value = explain.value;
            getPictrue();
            nextTick(function () {
                var _a = resetSize(proxy), imgHeight = _a.imgHeight, imgWidth = _a.imgWidth, barHeight = _a.barHeight, barWidth = _a.barWidth;
                setSize.imgHeight = imgHeight;
                setSize.imgWidth = imgWidth;
                setSize.barHeight = barHeight;
                setSize.barWidth = barWidth;
                proxy.$parent.$emit('ready', proxy);
            });
            window.removeEventListener("touchmove", function (e) {
                move(e);
            });
            window.removeEventListener("mousemove", function (e) {
                move(e);
            });
            //鼠标松开
            window.removeEventListener("touchend", function () {
                end();
            });
            window.removeEventListener("mouseup", function () {
                end();
            });
            window.addEventListener("touchmove", function (e) {
                move(e);
            });
            window.addEventListener("mousemove", function (e) {
                move(e);
            });
            //鼠标松开
            window.addEventListener("touchend", function () {
                end();
            });
            window.addEventListener("mouseup", function () {
                end();
            });
        }
        watch(type, function () {
            init();
        });
        onMounted(function () {
            // 禁止拖拽
            init();
            proxy.$el.onselectstart = function () {
                return false;
            };
        });
        //鼠标按下
        function start(e) {
            e = e || window.event;
            if (!e.touches) { //兼容PC端 
                var x = e.clientX;
            }
            else { //兼容移动端
                var x = e.touches[0].pageX;
            }
            // console.log(barArea);
            startLeft.value = Math.floor(x - barArea.value.getBoundingClientRect().left);
            startMoveTime.value = +new Date(); //开始滑动的时间
            if (isEnd.value == false) {
                text.value = '';
                moveBlockBackgroundColor.value = '#337ab7';
                leftBarBorderColor.value = '#337AB7';
                iconColor.value = '#fff';
                e.stopPropagation();
                status.value = true;
            }
        }
        //鼠标移动
        function move(e) {
            e = e || window.event;
            if (status.value && isEnd.value == false) {
                if (!e.touches) { //兼容PC端 
                    var x = e.clientX;
                }
                else { //兼容移动端
                    var x = e.touches[0].pageX;
                }
                var bar_area_left = barArea.value.getBoundingClientRect().left;
                var move_block_left = x - bar_area_left; //小方块相对于父元素的left值
                if (move_block_left >= barArea.value.offsetWidth - parseInt(parseInt(blockSize.value.width) / 2) - 2) {
                    move_block_left = barArea.value.offsetWidth - parseInt(parseInt(blockSize.value.width) / 2) - 2;
                }
                if (move_block_left <= 0) {
                    move_block_left = parseInt(parseInt(blockSize.value.width) / 2);
                }
                //拖动后小方块的left值
                moveBlockLeft.value = (move_block_left - startLeft.value) + "px";
                leftBarWidth.value = (move_block_left - startLeft.value) + "px";
            }
        }
        //鼠标松开
        function end() {
            endMovetime.value = +new Date();
            //判断是否重合
            if (status.value && isEnd.value == false) {
                var moveLeftDistance = parseInt((moveBlockLeft.value || '').replace('px', ''));
                moveLeftDistance = moveLeftDistance * 310 / parseInt(setSize.imgWidth);
                var data = {
                    captchaType: captchaType.value,
                    "pointJson": secretKey.value ? aesEncrypt(JSON.stringify({ x: moveLeftDistance, y: 5.0 }), secretKey.value) : JSON.stringify({ x: moveLeftDistance, y: 5.0 }),
                    "token": backToken.value
                };
                reqCheck(data).then(function (res) {
                    if (res.repCode == "0000") {
                        moveBlockBackgroundColor.value = '#5cb85c';
                        leftBarBorderColor.value = '#5cb85c';
                        iconColor.value = '#fff';
                        iconClass.value = 'icon-check';
                        showRefresh.value = false;
                        isEnd.value = true;
                        if (mode.value == 'pop') {
                            setTimeout(function () {
                                proxy.$parent.clickShow = false;
                                refresh();
                            }, 1500);
                        }
                        passFlag.value = true;
                        tipWords.value = "".concat(((endMovetime.value - startMoveTime.value) / 1000).toFixed(2), "s\u9A8C\u8BC1\u6210\u529F");
                        var captchaVerification = secretKey.value ? aesEncrypt(backToken.value + '---' + JSON.stringify({ x: moveLeftDistance, y: 5.0 }), secretKey.value) : backToken.value + '---' + JSON.stringify({ x: moveLeftDistance, y: 5.0 });
                        setTimeout(function () {
                            tipWords.value = "";
                            proxy.$parent.closeBox();
                            proxy.$parent.$emit('success', { captchaVerification: captchaVerification });
                        }, 1000);
                    }
                    else {
                        moveBlockBackgroundColor.value = '#d9534f';
                        leftBarBorderColor.value = '#d9534f';
                        iconColor.value = '#fff';
                        iconClass.value = 'icon-close';
                        passFlag.value = false;
                        setTimeout(function () {
                            refresh();
                        }, 1000);
                        proxy.$parent.$emit('error', proxy);
                        tipWords.value = "验证失败";
                        setTimeout(function () {
                            tipWords.value = "";
                        }, 1000);
                    }
                });
                status.value = false;
            }
        }
        var refresh = function () {
            showRefresh.value = true;
            finishText.value = '';
            transitionLeft.value = 'left .3s';
            moveBlockLeft.value = 0;
            leftBarWidth.value = undefined;
            transitionWidth.value = 'width .3s';
            leftBarBorderColor.value = '#ddd';
            moveBlockBackgroundColor.value = '#fff';
            iconColor.value = '#000';
            iconClass.value = 'icon-right';
            isEnd.value = false;
            getPictrue();
            setTimeout(function () {
                transitionWidth.value = '';
                transitionLeft.value = '';
                text.value = explain.value;
            }, 300);
        };
        // 请求背景图片和验证图片
        function getPictrue() {
            var data = {
                captchaType: captchaType.value
            };
            reqGet(data).then(function (res) {
                if (res.repCode == "0000") {
                    backImgBase.value = res.repData.originalImageBase64;
                    blockBackImgBase.value = res.repData.jigsawImageBase64;
                    backToken.value = res.repData.token;
                    secretKey.value = res.repData.secretKey;
                }
                else {
                    tipWords.value = res.repMsg;
                }
            });
        }
        return {
            secretKey: secretKey, //后端返回的ase加密秘钥
            passFlag: passFlag, //是否通过的标识
            backImgBase: backImgBase, //验证码背景图片
            blockBackImgBase: blockBackImgBase, //验证滑块的背景图片
            backToken: backToken, //后端返回的唯一token值
            startMoveTime: startMoveTime, //移动开始的时间
            endMovetime: endMovetime, //移动结束的时间
            tipsBackColor: tipsBackColor, //提示词的背景颜色
            tipWords: tipWords,
            text: text,
            finishText: finishText,
            setSize: setSize,
            top: top,
            left: left,
            moveBlockLeft: moveBlockLeft,
            leftBarWidth: leftBarWidth,
            // 移动中样式
            moveBlockBackgroundColor: moveBlockBackgroundColor,
            leftBarBorderColor: leftBarBorderColor,
            iconColor: iconColor,
            iconClass: iconClass,
            status: status, //鼠标状态
            isEnd: isEnd, //是够验证完成
            showRefresh: showRefresh,
            transitionLeft: transitionLeft,
            transitionWidth: transitionWidth,
            barArea: barArea,
            refresh: refresh,
            start: start
        };
    },
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
if (__VLS_ctx.type === '2') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-img-out" }, { style: ({ height: (parseInt(__VLS_ctx.setSize.imgHeight) + __VLS_ctx.vSpace) + 'px' }) }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-img-panel" }, { style: ({ width: __VLS_ctx.setSize.imgWidth,
            height: __VLS_ctx.setSize.imgHeight, }) }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.img, __VLS_intrinsicElements.img)(__assign({ src: ('data:image/png;base64,' + __VLS_ctx.backImgBase), alt: "" }, { style: {} }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ onClick: (__VLS_ctx.refresh) }, { class: "verify-refresh" }));
    __VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (__VLS_ctx.showRefresh) }), null, null);
    __VLS_asFunctionalElement(__VLS_intrinsicElements.i, __VLS_intrinsicElements.i)(__assign({ class: "iconfont icon-refresh" }));
    var __VLS_0 = {}.transition;
    /** @type {[typeof __VLS_components.Transition, typeof __VLS_components.transition, typeof __VLS_components.Transition, typeof __VLS_components.transition, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({
        name: "tips",
    }));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{
            name: "tips",
        }], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    if (__VLS_ctx.tipWords) {
        __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "verify-tips" }, { class: (__VLS_ctx.passFlag ? 'suc-bg' : 'err-bg') }));
        (__VLS_ctx.tipWords);
    }
    var __VLS_3;
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-bar-area" }, { style: ({ width: __VLS_ctx.setSize.imgWidth,
        height: __VLS_ctx.barSize.height,
        'line-height': __VLS_ctx.barSize.height }) }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "verify-msg" }));
__VLS_asFunctionalDirective(__VLS_directives.vText)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (__VLS_ctx.text) }), null, null);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-left-bar" }, { style: ({ width: (__VLS_ctx.leftBarWidth !== undefined) ? __VLS_ctx.leftBarWidth : __VLS_ctx.barSize.height, height: __VLS_ctx.barSize.height, 'border-color': __VLS_ctx.leftBarBorderColor, transaction: __VLS_ctx.transitionWidth }) }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "verify-msg" }));
__VLS_asFunctionalDirective(__VLS_directives.vText)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (__VLS_ctx.finishText) }), null, null);
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign(__assign({ onTouchstart: (__VLS_ctx.start) }, { onMousedown: (__VLS_ctx.start) }), { class: "verify-move-block" }), { style: ({ width: __VLS_ctx.barSize.height, height: __VLS_ctx.barSize.height, 'background-color': __VLS_ctx.moveBlockBackgroundColor, left: __VLS_ctx.moveBlockLeft, transition: __VLS_ctx.transitionLeft }) }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.i, __VLS_intrinsicElements.i)(__assign({ class: (['verify-icon iconfont', __VLS_ctx.iconClass]) }, { style: ({ color: __VLS_ctx.iconColor }) }));
if (__VLS_ctx.type === '2') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-sub-block" }, { style: ({ 'width': Math.floor(parseInt(__VLS_ctx.setSize.imgWidth) * 47 / 310) + 'px',
            'height': __VLS_ctx.setSize.imgHeight,
            'top': '-' + (parseInt(__VLS_ctx.setSize.imgHeight) + __VLS_ctx.vSpace) + 'px',
            'background-size': __VLS_ctx.setSize.imgWidth + ' ' + __VLS_ctx.setSize.imgHeight,
        }) }));
    __VLS_asFunctionalElement(__VLS_intrinsicElements.img, __VLS_intrinsicElements.img)(__assign({ src: ('data:image/png;base64,' + __VLS_ctx.blockBackImgBase), alt: "" }, { style: {} }));
}
/** @type {__VLS_StyleScopedClasses['verify-img-out']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-img-panel']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-refresh']} */ ;
/** @type {__VLS_StyleScopedClasses['iconfont']} */ ;
/** @type {__VLS_StyleScopedClasses['icon-refresh']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-tips']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-bar-area']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-msg']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-left-bar']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-msg']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-move-block']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-icon']} */ ;
/** @type {__VLS_StyleScopedClasses['iconfont']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-sub-block']} */ ;
var __VLS_dollars;
var __VLS_self;
