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
import { getMailMessageConfig, getMailServiceConfig, getMailTemplateDetail, getMailTemplateList, saveMailMessageConfig, saveMailServiceConfig, updateMailTemplate, } from "@/api/setting";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
var activeTab = ref("mail_template");
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
    handleTabInit(tabKey);
};
var handleTabInit = function (key) {
    switch (key) {
        case "mail_template":
            handleGetMailTemplateList();
            break;
        case "mail_service":
            handleGetMailServiceConfig();
            handleGetMailMessageConfig();
            break;
    }
};
/**  邮件模版 */
var mailTemplateList = reactive([]);
var handleGetMailTemplateList = function () {
    getMailTemplateList()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            mailTemplateList.length = 0;
            mailTemplateList.push.apply(mailTemplateList, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取邮件模版列表");
    });
};
/**
 * 邮件模版详情
 */
var editMailTemplateDrawerVisible = ref(false);
var mailTemplateDetailForm = reactive({
    id: undefined,
    name: undefined,
    content: undefined,
    parameters: undefined,
    subject: undefined,
    sender: undefined,
});
var mailTemplateDetailFormRef = ref(null);
var mailTemplateDetailFormRules = {
    subject: [{ required: true, message: "主题未填写" }],
    sender: [{ required: true, message: "发件人未填写" }],
    content: [{ required: true, message: "模版未填写" }],
};
/**
 * 前往编辑邮件模版
 */
var handleToEditMailTemplate = function (id) { return __awaiter(void 0, void 0, void 0, function () {
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, handleGetMailTemplateDetail(id)];
            case 1:
                _a.sent();
                editMailTemplateDrawerVisible.value = true;
                return [2 /*return*/];
        }
    });
}); };
/**
 * 获取邮件模版详情
 */
var handleGetMailTemplateDetail = function (id) { return __awaiter(void 0, void 0, void 0, function () {
    return __generator(this, function (_a) {
        getMailTemplateDetail(id)
            .then(function (result) {
            handleApiSuccess(result, function (data) {
                mailTemplateDetailForm.id = data.id;
                mailTemplateDetailForm.name = data.name;
                mailTemplateDetailForm.content = data.content;
                mailTemplateDetailForm.parameters = data.parameters;
                mailTemplateDetailForm.subject = data.subject;
                mailTemplateDetailForm.sender = data.sender;
            });
        })
            .catch(function (err) {
            handleApiError(err, "获取邮件模版详情");
        });
        return [2 /*return*/];
    });
}); };
/**
 * 重置邮件模版详情
 */
var handleResetMailTemplateDetailForm = function () {
    handleGetMailTemplateDetail(mailTemplateDetailForm.id);
    mailTemplateDetailFormRef.value.resetFields();
};
/**
 * 更新邮件模版
 */
var handleMailTemplateDetailFormSubmit = function () {
    updateMailTemplate(mailTemplateDetailForm)
        .then(function (result) {
        handleApiSuccess(result, function () {
            handleGetMailTemplateDetail(mailTemplateDetailForm.id);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新邮件模版");
    });
};
/** 邮件服务配置 */
var mailServiceConfigForm = reactive({
    host: undefined,
    port: undefined,
    username: undefined,
    password: undefined,
    sslEnable: false,
});
var mailServiceConfigFormRef = ref(null);
var mailServiceConfigFormRules = {
    host: [{ required: true, message: "主机未填写" }],
    port: [{ required: true, message: "端口未填写" }],
    username: [{ required: true, message: "用户名未填写" }],
    password: [{ required: true, message: "密码未填写" }],
    sslEnable: [{ required: true, message: "是否启用SSL未选择" }],
};
var handleResetMailServiceConfigForm = function () {
    mailServiceConfigFormRef.value.resetFields();
    handleGetMailServiceConfig();
};
/**
 * 获取邮件服务配置
 */
var handleGetMailServiceConfig = function () {
    getMailServiceConfig()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            mailServiceConfigForm.host = data.host;
            mailServiceConfigForm.port = data.port;
            mailServiceConfigForm.username = data.username;
            mailServiceConfigForm.password = data.password;
            mailServiceConfigForm.sslEnable = data.sslEnable || false;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取邮件服务配置");
    });
};
/**
 * 保存邮件服务配置
 */
var handleMailServiceConfigFormSubmit = function () {
    saveMailServiceConfig(mailServiceConfigForm)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetMailServiceConfig();
        });
    })
        .catch(function (err) {
        handleApiError(err, "保存邮件服务配置");
    });
};
/** 邮件消息配置 */
var mailMessageConfigForm = reactive({
    codeLive: undefined,
});
var mailMessageConfigFormRef = ref(null);
var mailMessageConfigFormRules = {
    codeLive: [{ required: true, message: "验证码有效期未填写" }],
};
var handleResetMailMessageConfigForm = function () {
    mailMessageConfigFormRef.value.resetFields();
    handleGetMailMessageConfig();
};
/**
 * 获取邮件消息配置
 */
var handleGetMailMessageConfig = function () {
    getMailMessageConfig()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            mailMessageConfigForm.codeLive = data.codeLive;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取邮件消息配置");
    });
};
/**
 * 保存邮件消息配置
 */
var handleMailMessageConfigFormSubmit = function () {
    saveMailMessageConfig(mailMessageConfigForm)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetMailMessageConfig();
        });
    })
        .catch(function (err) {
        handleApiError(err, "保存邮件消息配置");
    });
};
export default defineComponent({
    setup: function () {
        var tab = getQueryString("active_tab");
        var globalVariables = useGlobalVariablesStore();
        onMounted(function () {
            activeTab.value = tab || "mail_template";
            handleTabInit(activeTab.value);
        });
        return {
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            globalVariables: globalVariables,
            mailTemplateList: mailTemplateList,
            handleToEditMailTemplate: handleToEditMailTemplate,
            editMailTemplateDrawerVisible: editMailTemplateDrawerVisible,
            mailTemplateDetailForm: mailTemplateDetailForm,
            mailTemplateDetailFormRules: mailTemplateDetailFormRules,
            handleResetMailTemplateDetailForm: handleResetMailTemplateDetailForm,
            handleMailTemplateDetailFormSubmit: handleMailTemplateDetailFormSubmit,
            mailServiceConfigForm: mailServiceConfigForm,
            mailServiceConfigFormRef: mailServiceConfigFormRef,
            mailServiceConfigFormRules: mailServiceConfigFormRules,
            handleResetMailServiceConfigForm: handleResetMailServiceConfigForm,
            handleMailServiceConfigFormSubmit: handleMailServiceConfigFormSubmit,
            mailMessageConfigForm: mailMessageConfigForm,
            mailMessageConfigFormRef: mailMessageConfigFormRef,
            mailMessageConfigFormRules: mailMessageConfigFormRules,
            handleResetMailMessageConfigForm: handleResetMailMessageConfigForm,
            handleMailMessageConfigFormSubmit: handleMailMessageConfigFormSubmit,
        };
    },
});
