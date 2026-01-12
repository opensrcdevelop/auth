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
import { getEnabledDictData } from "@/api/dict";
import { bindUser, getBoundIdentitySource, unbindUser, } from "@/api/identitySource";
import { changePwd, sendEmailCodeSubmit } from "@/api/login";
import { logoutSubmit } from "@/api/logout";
import { checkPasswordWithoutPolicy } from "@/api/setting";
import { bindEmail, getCurrentUser, getVisibleUserAttrs, sendBindEmailCode, unbindEmail, updateMyUserInfo, } from "@/api/user";
import router from "@/router";
import { AUTH_FAILURE, AUTH_SUCCESS, BINDING_EXISTS } from "@/util/constants";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Message, Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
import { useRoute } from "vue-router";
var activeTab = ref("user_info");
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
var loading = ref(false);
var handleTabInit = function (tabKey) {
    loading.value = true;
    var initPromises = [];
    switch (tabKey) {
        case "user_info":
            initPromises.push(handleGetUserInfo());
            initPromises.push(handleGetUserAttrs());
            break;
        case "account_binding":
            initPromises.push(handleGetUserInfo());
            initPromises.push(handleGetBoundIdentitySource());
            break;
    }
    if (initPromises.length > 0) {
        Promise.all(initPromises).finally(function () {
            loading.value = false;
        });
    }
    else {
        loading.value = false;
    }
};
// 控制台访问权限
var consoleAccess = ref(false);
/**
 * 跳转到控制台
 */
var handleToConsole = function () {
    router.push({
        path: "/",
    });
};
/**
 * 退出登录
 */
var handleLogout = function () {
    Modal.warning({
        title: "确定退出登录？",
        content: "",
        hideCancel: false,
        okButtonProps: {
            status: "warning",
        },
        onOk: function () {
            logoutSubmit()
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("退出成功");
                    localStorage.removeItem("accessToken");
                    // 跳转到登录页
                    router.push({
                        path: "/oauth2/redirect",
                    });
                });
            })
                .catch(function (err) {
                handleApiError(err, "退出登录");
            });
        },
    });
};
/** 用户名 */
var username = ref(undefined);
/** 用户信息 */
var userInfo = reactive({});
/** 用户属性 */
var userAttrs = reactive([]);
/** 字典数据值 */
var allDictDatas = reactive({});
/**
 * 获取用户信息
 */
var handleGetUserInfo = function () {
    getCurrentUser()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            username.value = data.username;
            consoleAccess.value = data.consoleAccess;
            Object.assign(userInfo, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户信息");
    });
};
/**
 * 获取用户属性
 */
var handleGetUserAttrs = function () { return __awaiter(void 0, void 0, void 0, function () {
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, getVisibleUserAttrs()
                    .then(function (result) {
                    handleApiSuccess(result, function (data) { return __awaiter(void 0, void 0, void 0, function () {
                        var userIdIndex, userNameIndex, emailIndex, phoneIndex;
                        return __generator(this, function (_a) {
                            userAttrs.length = 0;
                            userAttrs.push.apply(userAttrs, data);
                            userIdIndex = userAttrs.findIndex(function (item) { return item.key === "userId"; });
                            if (userIdIndex > -1) {
                                userAttrs.splice(0, 0, userAttrs.splice(userIdIndex, 1)[0]);
                            }
                            userNameIndex = userAttrs.findIndex(function (item) { return item.key === "username"; });
                            if (userNameIndex > -1) {
                                userAttrs.splice(1, 0, userAttrs.splice(userNameIndex, 1)[0]);
                            }
                            emailIndex = userAttrs.findIndex(function (item) { return item.key === "emailAddress"; });
                            if (emailIndex > -1) {
                                userAttrs.splice(2, 0, userAttrs.splice(emailIndex, 1)[0]);
                            }
                            phoneIndex = userAttrs.findIndex(function (item) { return item.key === "phoneNumber"; });
                            if (phoneIndex > -1) {
                                userAttrs.splice(3, 0, userAttrs.splice(phoneIndex, 1)[0]);
                            }
                            return [2 /*return*/];
                        });
                    }); });
                })
                    .catch(function (err) {
                    handleApiError(err, "获取可见的用户属性");
                })];
            case 1:
                _a.sent();
                handleGetAllEnabledDictData();
                return [2 /*return*/];
        }
    });
}); };
/**
 * 获取所有启用的字典数据
 */
var handleGetAllEnabledDictData = function () { return __awaiter(void 0, void 0, void 0, function () {
    var getEnabledDictDataPromises;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                getEnabledDictDataPromises = [];
                userAttrs.forEach(function (item) {
                    if (item.dataType === "DICT" && item.dictId) {
                        allDictDatas[item.key] = [];
                        getEnabledDictDataPromises.push(handleGetEnabledDictData(item.key, item.dictId));
                    }
                });
                if (!(getEnabledDictDataPromises.length > 0)) return [3 /*break*/, 2];
                return [4 /*yield*/, Promise.all(getEnabledDictDataPromises)];
            case 1:
                _a.sent();
                _a.label = 2;
            case 2: return [2 /*return*/];
        }
    });
}); };
/**
 * 获取启用的字典数据
 */
var handleGetEnabledDictData = function (attrKey, dictId) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, getEnabledDictData(dictId)];
            case 1:
                result = _a.sent();
                handleApiSuccess(result, function (data) {
                    var _a;
                    allDictDatas[attrKey].length = 0;
                    (_a = allDictDatas[attrKey]).push.apply(_a, data);
                });
                return [3 /*break*/, 3];
            case 2:
                err_1 = _a.sent();
                handleApiError(err_1, "获取启用的字典数据");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
/**
 * 更新个人信息
 */
var handleUpdateMyUserInfo = function () {
    if (!userInfo["username"] || userInfo["username"].trim() === "") {
        Message.warning("用户名不能为空");
        return;
    }
    loading.value = true;
    updateMyUserInfo(userInfo)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetUserAttrs();
            handleGetUserInfo();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新个人信息");
    })
        .finally(function () {
        loading.value = false;
    });
};
/** 修改密码对话框 */
var changePwdModalVisivle = ref(false);
var changePwdForm = reactive({
    rawPwd: "",
    newPwd: "",
    confirmPwd: "",
});
var changePwdFormRef = ref();
var changePwdFormSubmitLoading = ref(false);
var changePwdFormRules = {
    rawPwd: [{ required: true, message: "原始密码未填写" }],
    newPwd: [{ required: true, message: "新密码未填写" }],
    confirmPwd: [
        { required: true, message: "确认新密码未填写" },
        {
            validator: function (value, cb) {
                if (value !== changePwdForm.newPwd) {
                    cb("两次输入的密码不一致");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
/**
 * 打开修改密码对话框
 */
var handleOpenChangePwdModal = function () {
    changePwdModalVisivle.value = true;
};
/**
 * 关闭修改密码对话框
 */
var handleCloseChangePwdModal = function () {
    changePwdModalVisivle.value = false;
    changePwdFormRef.value.resetFields();
    passwordCheckerRef.value.setPassword("");
};
/**
 * 提交修改密码表单
 *
 * @param formData 修改密码表单
 */
var handleSubmitChangePwdForm = function (formData) {
    if (!checkPasswordRes.valid) {
        return;
    }
    changePwdFormSubmitLoading.value = true;
    changePwd(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("密码修改成功");
            handleCloseChangePwdModal();
        });
    })
        .catch(function (err) {
        handleApiError(err, "修改密码");
    })
        .finally(function () {
        changePwdFormSubmitLoading.value = false;
    });
};
/**
 * 密码检查
 */
var passwordCheckerRef = ref(null);
var checkPasswordLoading = ref(false);
var checkPasswordRes = reactive({
    valid: false,
    errorMessage: undefined,
    ruleResults: undefined,
});
var handleCheckPassword = function (password) {
    checkPasswordLoading.value = true;
    changePwdForm.newPwd = password;
    checkPasswordWithoutPolicy({
        password: password,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            checkPasswordRes.valid = data.valid;
            checkPasswordRes.errorMessage = data.errorMessage;
            if (data.ruleResults) {
                checkPasswordRes.ruleResults = data.ruleResults;
            }
            else {
                checkPasswordRes.ruleResults = [];
            }
            checkPasswordLoading.value = false;
        });
    })
        .catch(function (err) {
        handleApiError(err, "密码检查");
        checkPasswordLoading.value = false;
    });
};
/** 绑定 / 解绑邮箱对话框 */
var bindOrUnbindEmailModalVisivle = ref(false);
var isBinding = ref(true);
var bindOrUnbindEmailFormSubmitLoading = ref(false);
var bindOrUnbindEmailFormRef = ref();
var bindOrUnbindEmailForm = reactive({
    email: undefined,
    code: undefined,
});
var bindOrUnbindEmailFormRules = {
    email: [
        {
            required: true,
            message: "邮箱未填写",
        },
    ],
    code: [
        {
            required: true,
            message: "验证码未填写",
        },
    ],
};
/**
 * 打开绑定邮箱对话框
 */
var handleOpenBindEmailModal = function () {
    isBinding.value = true;
    bindOrUnbindEmailModalVisivle.value = true;
};
/**
 * 打开解绑邮箱对话框
 */
var handleOpenUnbindEmailModal = function () {
    isBinding.value = false;
    bindOrUnbindEmailForm.email = userInfo["emailAddress"];
    bindOrUnbindEmailModalVisivle.value = true;
};
/**
 * 关闭绑定 / 解绑邮箱对话框
 */
var handleCoseBindOrUnbindEmailModal = function () {
    bindOrUnbindEmailModalVisivle.value = false;
    bindOrUnbindEmailFormRef.value.resetFields();
};
/**
 * 提交绑定 / 解绑邮箱表单
 */
var handleBindOrUnbindEmailFormSubmit = function () {
    bindOrUnbindEmailFormRef.value.validate(function (err) { return __awaiter(void 0, void 0, void 0, function () {
        var err_2;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    if (!!err) return [3 /*break*/, 8];
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 6, 7, 8]);
                    bindOrUnbindEmailFormSubmitLoading.value = true;
                    if (!isBinding.value) return [3 /*break*/, 3];
                    return [4 /*yield*/, bindEmail(bindOrUnbindEmailForm)];
                case 2:
                    _a.sent();
                    Notification.success("绑定邮箱成功");
                    handleGetUserInfo();
                    return [3 /*break*/, 5];
                case 3: return [4 /*yield*/, unbindEmail(bindOrUnbindEmailForm)];
                case 4:
                    _a.sent();
                    Notification.success("解绑邮箱成功");
                    handleGetUserInfo();
                    _a.label = 5;
                case 5:
                    handleCoseBindOrUnbindEmailModal();
                    return [3 /*break*/, 8];
                case 6:
                    err_2 = _a.sent();
                    handleApiError(err_2, "绑定 / 解绑邮箱");
                    return [3 /*break*/, 8];
                case 7:
                    bindOrUnbindEmailFormSubmitLoading.value = false;
                    return [7 /*endfinally*/];
                case 8: return [2 /*return*/];
            }
        });
    }); });
};
/** 发送邮箱验证码  */
var sendEmailCodeDisable = ref(false);
var sendEmailCodeBtnText = ref("发送验证码");
var remainingTime = 60;
var sendEmailCodeTimer;
var handleSendEmailCode = function () {
    if (!sendEmailCodeDisable.value) {
        bindOrUnbindEmailFormRef.value.validateField("email", function (err) { return __awaiter(void 0, void 0, void 0, function () {
            var err_3;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!!err) return [3 /*break*/, 7];
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 6, , 7]);
                        if (!isBinding.value) return [3 /*break*/, 3];
                        return [4 /*yield*/, sendBindEmailCode(bindOrUnbindEmailForm.email)];
                    case 2:
                        _a.sent();
                        return [3 /*break*/, 5];
                    case 3: return [4 /*yield*/, sendEmailCodeSubmit(bindOrUnbindEmailForm.email)];
                    case 4:
                        _a.sent();
                        _a.label = 5;
                    case 5:
                        // 60s 倒计时
                        sendEmailCodeDisable.value = true;
                        sendEmailCodeBtnText.value = "".concat(remainingTime, "s \u540E\u91CD\u8BD5");
                        sendEmailCodeTimer = setInterval(function () {
                            remainingTime--;
                            sendEmailCodeBtnText.value = "".concat(remainingTime, "s \u540E\u91CD\u8BD5");
                            if (remainingTime < 0) {
                                clearInterval(sendEmailCodeTimer);
                                sendEmailCodeDisable.value = false;
                                sendEmailCodeBtnText.value = "发送验证码";
                                remainingTime = 60;
                            }
                        }, 1000);
                        return [3 /*break*/, 7];
                    case 6:
                        err_3 = _a.sent();
                        handleApiError(err_3, "发送验证码");
                        return [3 /*break*/, 7];
                    case 7: return [2 /*return*/];
                }
            });
        }); });
    }
};
/** 绑定的身份源 */
var boundIdentitySource = reactive([]);
/**
 * 获取绑定的身份源
 */
var handleGetBoundIdentitySource = function () {
    getBoundIdentitySource()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            boundIdentitySource.length = 0;
            boundIdentitySource.push.apply(boundIdentitySource, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取绑定的身份源");
    });
};
/**
 * 绑定第三方账号
 */
var authWindow;
var handleBindUser = function (identitySource) {
    loading.value = true;
    bindUser(identitySource.code)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            // 打开第三方认证窗口
            authWindow = window.open(data.authReqUri, "_blank", "width=600,height=600");
        });
    })
        .catch(function (err) {
        handleApiError(err, "绑定第三方账号");
    })
        .finally(function () {
        loading.value = false;
    });
};
/**
 * 处理第三方认证窗口响应
 */
var handleAuthWindowResponse = function (event) {
    if (event.data === AUTH_SUCCESS) {
        Notification.success("第三方账号绑定成功");
        authWindow.close();
        handleGetBoundIdentitySource();
    }
    if (event.data === AUTH_FAILURE) {
        Notification.error("第三方账号绑定失败");
        authWindow.close();
    }
    if (event.data === BINDING_EXISTS) {
        Notification.error("该第三方账号已绑定其他用户，请先解绑");
        authWindow.close();
    }
};
/**
 * 解绑第三方账号
 */
var handleUnbindUser = function (identitySource) {
    Modal.warning({
        title: "\u786E\u5B9A\u4E0E\u300C".concat(identitySource.name, "\u300D\u89E3\u9664\u7ED1\u5B9A\u5417\uFF1F"),
        content: "",
        hideCancel: false,
        okButtonProps: {
            status: "warning",
        },
        onOk: function () {
            unbindUser(identitySource.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("解绑成功");
                    handleGetBoundIdentitySource();
                });
            })
                .catch(function (err) {
                handleApiError(err, "绑定第三方账号");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var route = useRoute();
            if (route.query.active_tab) {
                activeTab.value = route.query.active_tab;
            }
            handleTabInit(activeTab.value);
            window.addEventListener("message", handleAuthWindowResponse);
        });
        return {
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            handleToConsole: handleToConsole,
            handleLogout: handleLogout,
            username: username,
            userInfo: userInfo,
            userAttrs: userAttrs,
            handleUpdateMyUserInfo: handleUpdateMyUserInfo,
            loading: loading,
            changePwdModalVisivle: changePwdModalVisivle,
            handleOpenChangePwdModal: handleOpenChangePwdModal,
            handleCloseChangePwdModal: handleCloseChangePwdModal,
            changePwdForm: changePwdForm,
            changePwdFormRef: changePwdFormRef,
            changePwdFormRules: changePwdFormRules,
            handleSubmitChangePwdForm: handleSubmitChangePwdForm,
            changePwdFormSubmitLoading: changePwdFormSubmitLoading,
            bindOrUnbindEmailModalVisivle: bindOrUnbindEmailModalVisivle,
            isBinding: isBinding,
            bindOrUnbindEmailFormSubmitLoading: bindOrUnbindEmailFormSubmitLoading,
            bindOrUnbindEmailFormRef: bindOrUnbindEmailFormRef,
            bindOrUnbindEmailForm: bindOrUnbindEmailForm,
            bindOrUnbindEmailFormRules: bindOrUnbindEmailFormRules,
            handleOpenBindEmailModal: handleOpenBindEmailModal,
            handleOpenUnbindEmailModal: handleOpenUnbindEmailModal,
            handleCoseBindOrUnbindEmailModal: handleCoseBindOrUnbindEmailModal,
            handleBindOrUnbindEmailFormSubmit: handleBindOrUnbindEmailFormSubmit,
            sendEmailCodeDisable: sendEmailCodeDisable,
            sendEmailCodeBtnText: sendEmailCodeBtnText,
            handleSendEmailCode: handleSendEmailCode,
            allDictDatas: allDictDatas,
            passwordCheckerRef: passwordCheckerRef,
            checkPasswordLoading: checkPasswordLoading,
            checkPasswordRes: checkPasswordRes,
            handleCheckPassword: handleCheckPassword,
            consoleAccess: consoleAccess,
            boundIdentitySource: boundIdentitySource,
            handleBindUser: handleBindUser,
            handleUnbindUser: handleUnbindUser,
        };
    },
});
