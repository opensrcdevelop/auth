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
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
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
var _a, _b;
import { getAnsweredSql, voteAnswer } from "@/api/chatbi";
import { copyToClipboard, handleApiError, handleApiSuccess } from "@/util/tool";
import { Message } from "@arco-design/web-vue";
var props = withDefaults(defineProps(), {
    message: {},
});
var emits = defineEmits();
var handleResendMessage = function () {
    emits("resendMessage", props.message.rewrittenQuestion);
};
var handleVoteAnswer = function (doneMessage, feedback) {
    var answerId = doneMessage.answerId;
    voteAnswer({
        answerId: answerId,
        feedback: (doneMessage === null || doneMessage === void 0 ? void 0 : doneMessage.feedback) === feedback ? undefined : feedback,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            doneMessage.feedback =
                (doneMessage === null || doneMessage === void 0 ? void 0 : doneMessage.feedback) === feedback ? undefined : feedback;
        });
    })
        .catch(function (err) {
        handleApiError(err, "反馈图表");
    });
};
var handleGetAnsweredSql = function (doneMessage) {
    getAnsweredSql(doneMessage.answerId).then(function (result) {
        handleApiSuccess(result, function (data) { return __awaiter(void 0, void 0, void 0, function () {
            var result_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!data.sql) return [3 /*break*/, 2];
                        return [4 /*yield*/, copyToClipboard(data.sql)];
                    case 1:
                        result_1 = _a.sent();
                        if (result_1) {
                            Message.success("复制成功");
                        }
                        else {
                            Message.error("复制失败");
                        }
                        return [3 /*break*/, 3];
                    case 2:
                        Message.warning("该回答没有生成 SQL");
                        _a.label = 3;
                    case 3: return [2 /*return*/];
                }
            });
        }); });
    }).catch(function (err) {
        handleApiError(err, "获取回答的 SQL");
    });
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
if (__VLS_ctx.message.type === 'DONE') {
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)({});
    __VLS_asFunctionalElement(__VLS_intrinsicElements.div, __VLS_intrinsicElements.div)(__assign({ class: "operator-container" }));
    var __VLS_0 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_1 = __VLS_asFunctionalComponent(__VLS_0, new __VLS_0({}));
    var __VLS_2 = __VLS_1.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_1), false));
    __VLS_3.slots.default;
    var __VLS_4 = {}.AButton;
    /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
    // @ts-ignore
    var __VLS_5 = __VLS_asFunctionalComponent(__VLS_4, new __VLS_4(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
    var __VLS_6 = __VLS_5.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_5), false));
    var __VLS_8 = void 0;
    var __VLS_9 = void 0;
    var __VLS_10 = void 0;
    var __VLS_11 = {
        onClick: (__VLS_ctx.handleResendMessage)
    };
    __VLS_7.slots.default;
    {
        var __VLS_thisSlot = __VLS_7.slots.icon;
        var __VLS_12 = {}.IconRefresh;
        /** @type {[typeof __VLS_components.IconRefresh, typeof __VLS_components.iconRefresh, ]} */ ;
        // @ts-ignore
        var __VLS_13 = __VLS_asFunctionalComponent(__VLS_12, new __VLS_12({}));
        var __VLS_14 = __VLS_13.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_13), false));
    }
    {
        var __VLS_thisSlot = __VLS_7.slots.default;
    }
    var __VLS_7;
    if (__VLS_ctx.message.answerId) {
        var __VLS_16 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_17 = __VLS_asFunctionalComponent(__VLS_16, new __VLS_16(__assign({ 'onClick': {} }, { type: "text", size: "mini" })));
        var __VLS_18 = __VLS_17.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { type: "text", size: "mini" })], __VLS_functionalComponentArgsRest(__VLS_17), false));
        var __VLS_20 = void 0;
        var __VLS_21 = void 0;
        var __VLS_22 = void 0;
        var __VLS_23 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.message.type === 'DONE'))
                    return;
                if (!(__VLS_ctx.message.answerId))
                    return;
                __VLS_ctx.handleGetAnsweredSql(__VLS_ctx.message);
            }
        };
        __VLS_19.slots.default;
        {
            var __VLS_thisSlot = __VLS_19.slots.icon;
            var __VLS_24 = {}.IconCopy;
            /** @type {[typeof __VLS_components.IconCopy, typeof __VLS_components.iconCopy, ]} */ ;
            // @ts-ignore
            var __VLS_25 = __VLS_asFunctionalComponent(__VLS_24, new __VLS_24({}));
            var __VLS_26 = __VLS_25.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_25), false));
        }
        {
            var __VLS_thisSlot = __VLS_19.slots.default;
        }
        var __VLS_19;
    }
    if (__VLS_ctx.message.answerId) {
        var __VLS_28 = {}.ADivider;
        /** @type {[typeof __VLS_components.ADivider, typeof __VLS_components.aDivider, ]} */ ;
        // @ts-ignore
        var __VLS_29 = __VLS_asFunctionalComponent(__VLS_28, new __VLS_28({
            direction: "vertical",
        }));
        var __VLS_30 = __VLS_29.apply(void 0, __spreadArray([{
                direction: "vertical",
            }], __VLS_functionalComponentArgsRest(__VLS_29), false));
    }
    if (__VLS_ctx.message.answerId) {
        var __VLS_32 = {}.ASpace;
        /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
        // @ts-ignore
        var __VLS_33 = __VLS_asFunctionalComponent(__VLS_32, new __VLS_32({}));
        var __VLS_34 = __VLS_33.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_33), false));
        __VLS_35.slots.default;
        var __VLS_36 = {}.ATooltip;
        /** @type {[typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, ]} */ ;
        // @ts-ignore
        var __VLS_37 = __VLS_asFunctionalComponent(__VLS_36, new __VLS_36({
            content: "喜欢",
            position: "bottom",
            mini: true,
        }));
        var __VLS_38 = __VLS_37.apply(void 0, __spreadArray([{
                content: "喜欢",
                position: "bottom",
                mini: true,
            }], __VLS_functionalComponentArgsRest(__VLS_37), false));
        __VLS_39.slots.default;
        var __VLS_40 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_41 = __VLS_asFunctionalComponent(__VLS_40, new __VLS_40(__assign({ 'onClick': {} }, { size: "mini", shape: "circle" })));
        var __VLS_42 = __VLS_41.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { size: "mini", shape: "circle" })], __VLS_functionalComponentArgsRest(__VLS_41), false));
        var __VLS_44 = void 0;
        var __VLS_45 = void 0;
        var __VLS_46 = void 0;
        var __VLS_47 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.message.type === 'DONE'))
                    return;
                if (!(__VLS_ctx.message.answerId))
                    return;
                __VLS_ctx.handleVoteAnswer(__VLS_ctx.message, 'LIKE');
            }
        };
        __VLS_43.slots.default;
        {
            var __VLS_thisSlot = __VLS_43.slots.icon;
            if (((_a = __VLS_ctx.message) === null || _a === void 0 ? void 0 : _a.feedback) === 'LIKE') {
                var __VLS_48 = {}.IconThumbUpFill;
                /** @type {[typeof __VLS_components.IconThumbUpFill, typeof __VLS_components.iconThumbUpFill, ]} */ ;
                // @ts-ignore
                var __VLS_49 = __VLS_asFunctionalComponent(__VLS_48, new __VLS_48({}));
                var __VLS_50 = __VLS_49.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_49), false));
            }
            else {
                var __VLS_52 = {}.IconThumbUp;
                /** @type {[typeof __VLS_components.IconThumbUp, typeof __VLS_components.iconThumbUp, ]} */ ;
                // @ts-ignore
                var __VLS_53 = __VLS_asFunctionalComponent(__VLS_52, new __VLS_52({}));
                var __VLS_54 = __VLS_53.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_53), false));
            }
        }
        var __VLS_43;
        var __VLS_39;
        var __VLS_56 = {}.ATooltip;
        /** @type {[typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, typeof __VLS_components.ATooltip, typeof __VLS_components.aTooltip, ]} */ ;
        // @ts-ignore
        var __VLS_57 = __VLS_asFunctionalComponent(__VLS_56, new __VLS_56({
            content: "不喜欢",
            position: "bottom",
            mini: true,
        }));
        var __VLS_58 = __VLS_57.apply(void 0, __spreadArray([{
                content: "不喜欢",
                position: "bottom",
                mini: true,
            }], __VLS_functionalComponentArgsRest(__VLS_57), false));
        __VLS_59.slots.default;
        var __VLS_60 = {}.AButton;
        /** @type {[typeof __VLS_components.AButton, typeof __VLS_components.aButton, typeof __VLS_components.AButton, typeof __VLS_components.aButton, ]} */ ;
        // @ts-ignore
        var __VLS_61 = __VLS_asFunctionalComponent(__VLS_60, new __VLS_60(__assign({ 'onClick': {} }, { size: "mini", shape: "circle" })));
        var __VLS_62 = __VLS_61.apply(void 0, __spreadArray([__assign({ 'onClick': {} }, { size: "mini", shape: "circle" })], __VLS_functionalComponentArgsRest(__VLS_61), false));
        var __VLS_64 = void 0;
        var __VLS_65 = void 0;
        var __VLS_66 = void 0;
        var __VLS_67 = {
            onClick: function () {
                var _a = [];
                for (var _i = 0; _i < arguments.length; _i++) {
                    _a[_i] = arguments[_i];
                }
                var $event = _a[0];
                if (!(__VLS_ctx.message.type === 'DONE'))
                    return;
                if (!(__VLS_ctx.message.answerId))
                    return;
                __VLS_ctx.handleVoteAnswer(__VLS_ctx.message, 'DISLIKE');
            }
        };
        __VLS_63.slots.default;
        {
            var __VLS_thisSlot = __VLS_63.slots.icon;
            if (((_b = __VLS_ctx.message) === null || _b === void 0 ? void 0 : _b.feedback) === 'DISLIKE') {
                var __VLS_68 = {}.IconThumbDownFill;
                /** @type {[typeof __VLS_components.IconThumbDownFill, typeof __VLS_components.iconThumbDownFill, ]} */ ;
                // @ts-ignore
                var __VLS_69 = __VLS_asFunctionalComponent(__VLS_68, new __VLS_68({}));
                var __VLS_70 = __VLS_69.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_69), false));
            }
            else {
                var __VLS_72 = {}.IconThumbDown;
                /** @type {[typeof __VLS_components.IconThumbDown, typeof __VLS_components.iconThumbDown, ]} */ ;
                // @ts-ignore
                var __VLS_73 = __VLS_asFunctionalComponent(__VLS_72, new __VLS_72({}));
                var __VLS_74 = __VLS_73.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_73), false));
            }
        }
        var __VLS_63;
        var __VLS_59;
        var __VLS_35;
    }
    var __VLS_3;
    var __VLS_76 = {}.ASpace;
    /** @type {[typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, typeof __VLS_components.ASpace, typeof __VLS_components.aSpace, ]} */ ;
    // @ts-ignore
    var __VLS_77 = __VLS_asFunctionalComponent(__VLS_76, new __VLS_76({}));
    var __VLS_78 = __VLS_77.apply(void 0, __spreadArray([{}], __VLS_functionalComponentArgsRest(__VLS_77), false));
    __VLS_79.slots.default;
    __VLS_asFunctionalElement(__VLS_intrinsicElements.span, __VLS_intrinsicElements.span)(__assign({ class: "time" }));
    (__VLS_ctx.message.time);
    var __VLS_79;
}
/** @type {__VLS_StyleScopedClasses['operator-container']} */ ;
/** @type {__VLS_StyleScopedClasses['time']} */ ;
var __VLS_dollars;
var __VLS_self = (await import('vue')).defineComponent({
    setup: function () {
        return {
            handleResendMessage: handleResendMessage,
            handleVoteAnswer: handleVoteAnswer,
            handleGetAnsweredSql: handleGetAnsweredSql,
        };
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
export default (await import('vue')).defineComponent({
    setup: function () {
        return {};
    },
    __typeEmits: {},
    __typeProps: {},
    props: {},
});
; /* PartiallyEnd: #4569/main.vue */
