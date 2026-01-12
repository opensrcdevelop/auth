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
import { deletePasswordPolicy, getPasswordPolicyList, getUpdatePasswordRemindLogList, updatePasswordPolicy, updatePasswordPolicyPriority, } from "@/api/setting";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
var activeTab = ref("password_policy");
/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
    handleTabInit(activeTab.value);
};
var handleTabInit = function (key) {
    switch (key) {
        case "password_policy":
            handleGetPasswordPolicyList();
            break;
        case "remind_logs":
            handleGetRemindLogList();
            break;
    }
};
/**
 * 跳转到创建密码策略页面
 */
var handleToCreatePasswordPolicy = function () {
    router.push({
        path: "/system_setting/password/create",
    });
};
/**
 * 密码策略列表
 */
var passwordPolicyList = reactive([]);
var handleGetPasswordPolicyList = function () {
    getPasswordPolicyList()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            passwordPolicyList.length = 0;
            passwordPolicyList.push.apply(passwordPolicyList, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取密码策略列表");
    });
};
/**
 * 获取密码强度标签
 */
var getPasswordStrengthLabel = function (strength) {
    var label = "";
    switch (strength) {
        case 0:
            label = "无要求";
            break;
        case 1:
            label = "低强度";
            break;
        case 2:
            label = "中强度";
            break;
        case 3:
            label = "高强度";
            break;
        case 4:
            label = "自定义强度";
            break;
    }
    return label;
};
/**
 * 更新密码策略执行顺序
 */
var handlTableChange = function (data) { return __awaiter(void 0, void 0, void 0, function () {
    var requestData, err_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                passwordPolicyList.length = 0;
                passwordPolicyList.push.apply(passwordPolicyList, data);
                requestData = data.map(function (item, index) {
                    return {
                        id: item.id,
                        priority: index,
                    };
                });
                _a.label = 1;
            case 1:
                _a.trys.push([1, 3, , 4]);
                return [4 /*yield*/, updatePasswordPolicyPriority(requestData)];
            case 2:
                _a.sent();
                Notification.success("更新执行顺序成功");
                return [3 /*break*/, 4];
            case 3:
                err_1 = _a.sent();
                handleApiError(err_1, "更新执行顺序");
                return [3 /*break*/, 4];
            case 4:
                handleGetPasswordPolicyList();
                return [2 /*return*/];
        }
    });
}); };
/**
 * 更新密码策略状态
 */
var handleUpdatePasswordPolicyState = function (data) {
    updatePasswordPolicy({
        id: data.id,
        enabled: data.enabled,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("更新密码策略状态成功");
            handleGetPasswordPolicyList();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新密码策略状态");
        handleGetPasswordPolicyList();
    });
};
/**
 * 跳转密码策略详情
 */
var handleToPasswordPolicyDetail = function (id) {
    router.push({
        path: "/system_setting/password/detail",
        query: {
            id: id
        },
    });
};
/**
 * 删除密码策略
 */
var handleDeletePasswordPolicy = function (passwordPolicy) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u5BC6\u7801\u7B56\u7565\u300C".concat(passwordPolicy.name, "\u300D\u5417\uFF1F"),
        content: "",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deletePasswordPolicy(passwordPolicy.id)
                .then(function (result) {
                Notification.success("删除成功");
                handleGetPasswordPolicyList();
            })
                .catch(function (err) {
                handleApiError(err, "删除密码策略");
            });
        },
    });
};
/**
 * 密码到期提醒记录列表
 */
var remindLogList = reactive([]);
var remindLogSearchKeyword = ref(null);
var remindLogListPagination;
/**
 * 获取密码到期提醒记录列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetRemindLogList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUpdatePasswordRemindLogList({
        page: page,
        size: size,
        keyword: remindLogSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            remindLogList.length = 0;
            remindLogList.push.apply(remindLogList, data.list);
            remindLogListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取密码到期提醒记录列表");
    });
};
/**
 * 跳转用户详情
 *
 * @param user 用户信息
 */
var handleToUserDetail = function (id) {
    router.push({
        path: "/user/detail",
        query: {
            id: id,
            active_tab: "user_info",
        },
    });
};
export default defineComponent({
    setup: function () {
        var tab = getQueryString("active_tab");
        remindLogListPagination = usePagination("remindLogList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetRemindLogList(page, size);
        });
        onMounted(function () {
            activeTab.value = tab || "password_policy";
            handleTabInit(activeTab.value);
        });
        return {
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            handleToCreatePasswordPolicy: handleToCreatePasswordPolicy,
            passwordPolicyList: passwordPolicyList,
            getPasswordStrengthLabel: getPasswordStrengthLabel,
            handlTableChange: handlTableChange,
            handleUpdatePasswordPolicyState: handleUpdatePasswordPolicyState,
            handleToPasswordPolicyDetail: handleToPasswordPolicyDetail,
            handleDeletePasswordPolicy: handleDeletePasswordPolicy,
            remindLogList: remindLogList,
            remindLogSearchKeyword: remindLogSearchKeyword,
            remindLogListPagination: remindLogListPagination,
            handleGetRemindLogList: handleGetRemindLogList,
            handleToUserDetail: handleToUserDetail,
        };
    },
});
