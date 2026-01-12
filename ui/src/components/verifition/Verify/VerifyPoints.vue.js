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
import { resetSize } from '../utils/util';
import { aesEncrypt } from "../utils/ase";
import { reqCheck, reqGet } from "../api/index";
import { getCurrentInstance, nextTick, onMounted, reactive, ref, toRefs } from 'vue';
export default (await import('vue')).defineComponent({
    name: 'VerifyPoints',
    props: {
        //弹出式pop，固定fixed
        mode: {
            type: String,
            default: 'fixed'
        },
        captchaType: {
            type: String,
        },
        //间隔
        vSpace: {
            type: Number,
            default: 5
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
        var _a = toRefs(props), mode = _a.mode, captchaType = _a.captchaType, vSpace = _a.vSpace, imgSize = _a.imgSize, barSize = _a.barSize;
        var proxy = getCurrentInstance().proxy;
        var secretKey = ref(''), //后端返回的ase加密秘钥
        checkNum = ref(3), //默认需要点击的字数
        fontPos = reactive([]), //选中的坐标信息
        checkPosArr = reactive([]), //用户点击的坐标
        num = ref(1), //点击的记数
        pointBackImgBase = ref(''), //后端获取到的背景图片
        poinTextList = reactive([]), //后端返回的点击字体顺序
        backToken = ref(''), //后端返回的token值
        setSize = reactive({
            imgHeight: 0,
            imgWidth: 0,
            barHeight: 0,
            barWidth: 0
        }), tempPoints = reactive([]), text = ref(''), barAreaColor = ref(undefined), barAreaBorderColor = ref(undefined), showRefresh = ref(true), bindingClick = ref(true);
        var init = function () {
            //加载页面
            fontPos.splice(0, fontPos.length);
            checkPosArr.splice(0, checkPosArr.length);
            num.value = 1;
            getPictrue();
            nextTick(function () {
                var _a = resetSize(proxy), imgHeight = _a.imgHeight, imgWidth = _a.imgWidth, barHeight = _a.barHeight, barWidth = _a.barWidth;
                setSize.imgHeight = imgHeight;
                setSize.imgWidth = imgWidth;
                setSize.barHeight = barHeight;
                setSize.barWidth = barWidth;
                proxy.$parent.$emit('ready', proxy);
            });
        };
        onMounted(function () {
            // 禁止拖拽
            init();
            proxy.$el.onselectstart = function () {
                return false;
            };
        });
        var canvas = ref(null);
        var canvasClick = function (e) {
            checkPosArr.push(getMousePos(canvas, e));
            if (num.value == checkNum.value) {
                num.value = createPoint(getMousePos(canvas, e));
                //按比例转换坐标值
                var arr = pointTransfrom(checkPosArr, setSize);
                checkPosArr.length = 0;
                checkPosArr.push.apply(checkPosArr, arr);
                //等创建坐标执行完
                setTimeout(function () {
                    // var flag = this.comparePos(this.fontPos, this.checkPosArr);
                    //发送后端请求
                    var captchaVerification = secretKey.value ? aesEncrypt(backToken.value + '---' + JSON.stringify(checkPosArr), secretKey.value) : backToken.value + '---' + JSON.stringify(checkPosArr);
                    var data = {
                        captchaType: captchaType.value,
                        "pointJson": secretKey.value ? aesEncrypt(JSON.stringify(checkPosArr), secretKey.value) : JSON.stringify(checkPosArr),
                        "token": backToken.value
                    };
                    reqCheck(data).then(function (res) {
                        if (res.repCode == "0000") {
                            barAreaColor.value = '#4cae4c';
                            barAreaBorderColor.value = '#5cb85c';
                            text.value = '验证成功';
                            bindingClick.value = false;
                            if (mode.value == 'pop') {
                                setTimeout(function () {
                                    proxy.$parent.clickShow = false;
                                    refresh();
                                }, 1500);
                            }
                            proxy.$parent.$emit('success', { captchaVerification: captchaVerification });
                        }
                        else {
                            proxy.$parent.$emit('error', proxy);
                            barAreaColor.value = '#d9534f';
                            barAreaBorderColor.value = '#d9534f';
                            text.value = '验证失败';
                            setTimeout(function () {
                                refresh();
                            }, 700);
                        }
                    });
                }, 400);
            }
            if (num.value < checkNum.value) {
                num.value = createPoint(getMousePos(canvas, e));
            }
        };
        //获取坐标
        var getMousePos = function (obj, e) {
            var x = e.offsetX;
            var y = e.offsetY;
            return { x: x, y: y };
        };
        //创建坐标点
        var createPoint = function (pos) {
            tempPoints.push(Object.assign({}, pos));
            return num.value + 1;
        };
        var refresh = function () {
            tempPoints.splice(0, tempPoints.length);
            barAreaColor.value = '#000';
            barAreaBorderColor.value = '#ddd';
            bindingClick.value = true;
            fontPos.splice(0, fontPos.length);
            checkPosArr.splice(0, checkPosArr.length);
            num.value = 1;
            getPictrue();
            text.value = '验证失败';
            showRefresh.value = true;
        };
        // 请求背景图片和验证图片
        function getPictrue() {
            var data = {
                captchaType: captchaType.value
            };
            reqGet(data).then(function (res) {
                if (res.repCode == "0000") {
                    pointBackImgBase.value = res.repData.originalImageBase64;
                    backToken.value = res.repData.token;
                    secretKey.value = res.repData.secretKey;
                    poinTextList.value = res.repData.wordList;
                    text.value = '请依次点击【' + poinTextList.value.join(",") + '】';
                }
                else {
                    text.value = res.repMsg;
                }
            });
        }
        //坐标转换函数
        var pointTransfrom = function (pointArr, imgSize) {
            var newPointArr = pointArr.map(function (p) {
                var x = Math.round(310 * p.x / parseInt(imgSize.imgWidth));
                var y = Math.round(155 * p.y / parseInt(imgSize.imgHeight));
                return { x: x, y: y };
            });
            return newPointArr;
        };
        return {
            secretKey: secretKey,
            checkNum: checkNum,
            fontPos: fontPos,
            checkPosArr: checkPosArr,
            num: num,
            pointBackImgBase: pointBackImgBase,
            poinTextList: poinTextList,
            backToken: backToken,
            setSize: setSize,
            tempPoints: tempPoints,
            text: text,
            barAreaColor: barAreaColor,
            barAreaBorderColor: barAreaBorderColor,
            showRefresh: showRefresh,
            bindingClick: bindingClick,
            init: init,
            canvas: canvas,
            canvasClick: canvasClick,
            getMousePos: getMousePos,
            createPoint: createPoint,
            refresh: refresh,
            getPictrue: getPictrue,
            pointTransfrom: pointTransfrom
        };
    },
});
var __VLS_ctx = {};
var __VLS_components;
var __VLS_directives;
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ style: {} }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-img-out" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-img-panel" }, { style: ({ 'width': __VLS_ctx.setSize.imgWidth,
        'height': __VLS_ctx.setSize.imgHeight,
        'background-size': __VLS_ctx.setSize.imgWidth + ' ' + __VLS_ctx.setSize.imgHeight,
        'margin-bottom': __VLS_ctx.vSpace + 'px' }) }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ onClick: (__VLS_ctx.refresh) }, { class: "verify-refresh" }), { style: {} }));
__VLS_asFunctionalDirective(__VLS_directives.vShow)(null, __assign(__assign({}, __VLS_directiveBindingRestFields), { value: (__VLS_ctx.showRefresh) }), null, null);
__VLS_asFunctionalElement(__VLS_intrinsicElements.i, __VLS_intrinsicElements.i)(__assign({ class: "iconfont icon-refresh" }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.img, __VLS_intrinsicElements.img)(__assign(__assign({ onClick: function () {
        var _a = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            _a[_i] = arguments[_i];
        }
        var $event = _a[0];
        __VLS_ctx.bindingClick ? __VLS_ctx.canvasClick($event) : undefined;
    } }, { src: ('data:image/png;base64,' + __VLS_ctx.pointBackImgBase), ref: "canvas", alt: "" }), { style: {} }));
/** @type {typeof __VLS_ctx.canvas} */ ;
for (var _i = 0, _a = __VLS_getVForSourceType((__VLS_ctx.tempPoints)); _i < _a.length; _i++) {
    var _b = _a[_i], tempPoint = _b[0], index = _b[1];
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign(__assign({ key: (index) }, { class: "point-area" }), { style: ({
            'background-color': '#1abd6c',
            color: '#fff',
            'z-index': 9999,
            width: '20px',
            height: '20px',
            'text-align': 'center',
            'line-height': '20px',
            'border-radius': '50%',
            position: 'absolute',
            top: parseInt(tempPoint.y - 10) + 'px',
            left: parseInt(tempPoint.x - 10) + 'px'
        }) }));
    (index + 1);
}
__VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "verify-bar-area" }, { style: ({ 'width': __VLS_ctx.setSize.imgWidth,
        'color': this.barAreaColor,
        'border-color': this.barAreaBorderColor,
        'line-height': this.barSize.height }) }));
__VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "verify-msg" }));
(__VLS_ctx.text);
/** @type {__VLS_StyleScopedClasses['verify-img-out']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-img-panel']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-refresh']} */ ;
/** @type {__VLS_StyleScopedClasses['iconfont']} */ ;
/** @type {__VLS_StyleScopedClasses['icon-refresh']} */ ;
/** @type {__VLS_StyleScopedClasses['point-area']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-bar-area']} */ ;
/** @type {__VLS_StyleScopedClasses['verify-msg']} */ ;
var __VLS_dollars;
var __VLS_self;
