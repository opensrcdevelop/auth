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
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { checkCode, emailLoginSubmit, loginSubmit, resetPwd, sendEmailCodeSubmit, totpValidSubmit, } from "@/api/login";
import { logoutSubmit } from "@/api/logout";
import router from "@/router";
import { TENANT_NAME } from "@/util/constants";
import { checkPasswordWithoutPolicy } from "@/api/setting";
import FederationLogin from "./components/FederationLogin.vue";
/** 租户名称 */
var tenantName = ref(undefined);
/** 记住我 */
var rememberMe = ref(false);
var passwordLoginForm = reactive({
    username: undefined,
    password: undefined,
    captchaVerification: undefined
});
var passwordLoginRules = {
    username: [
        {
            required: true,
            message: "账号未填写",
        },
    ],
    password: [
        {
            required: true,
            message: "密码未填写",
        },
    ],
};
var toMfa = ref(false);
var toBind = ref(false);
var qrCodeData = ref("");
var totpValidForm = reactive({
    code: "",
});
var loginLoading = ref(false);
var mfaValidLoading = ref(false);
var captchaVerifyRef = ref();
/**
 * 返回登录页
 */
var backToLogin = function () {
    logoutSubmit()
        .then(function (result) {
        handleApiSuccess(result, function () {
            toMfa.value = false;
            passwordLoginForm.password = "";
            passwordLoginForm.username = "";
            totpValidForm.code = "";
        });
    })
        .catch(function (err) {
        Notification.error("\u8FD4\u56DE\u767B\u5F55\u9875\u5931\u8D25: ".concat(err.message));
    });
};
/**
 * 打开图像验证码
 */
var openCaptchaVerify = function () {
    captchaVerifyRef.value.show();
};
/**
 * 提交密码登录表单
 *
 * @param captchaVerification 校验码
 */
var handlePasswordLoginFromSubmit = function (captchaVerification) {
    loginLoading.value = true;
    passwordLoginForm.captchaVerification =
        captchaVerification.captchaVerification;
    loginSubmit(__assign(__assign({}, passwordLoginForm), { rememberMe: rememberMe.value }))
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            handleLoginResult(data, "password");
        });
    })
        .catch(function (err) {
        handleApiError(err, "登录");
    })
        .finally(function () {
        loginLoading.value = false;
    });
};
/**
 * Totp 安全动态码验证
 *
 * @param code
 */
var handleTotpValidSubmit = function (code) {
    mfaValidLoading.value = true;
    totpValidSubmit(totpValidForm)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (data.valid) {
                toTarget();
            }
            else {
                Notification.warning("安全码错误，请重新输入");
            }
        });
    })
        .catch(function (err) {
        handleApiError(err, "MFA认证");
    })
        .finally(function () {
        mfaValidLoading.value = false;
    });
};
/**
 * 跳转至目标路径
 */
function toTarget() {
    return __awaiter(this, void 0, void 0, function () {
        var target;
        return __generator(this, function (_a) {
            target = getQueryString("target");
            if (target) {
                window.location.href = target;
            }
            else {
                router.push({
                    path: "/",
                });
            }
            return [2 /*return*/];
        });
    });
}
var emailLoginFormRef = ref();
var emailLoginForm = reactive({
    email: undefined,
    code: undefined,
    rememberMe: false
});
var emailLoginFormRules = reactive({
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
});
var sendEmailCodeDisable = ref(false);
var sendEmailCodeBtnText = ref("发送验证码");
var remainingTime = 60;
var sendEmailCodeTimer;
/**
 * 发送邮箱验证码
 */
var handleSendEmailCode = function () {
    if (!sendEmailCodeDisable.value) {
        emailLoginFormRef.value.validateField("email", function (err) {
            if (!err) {
                sendEmailCodeSubmit(emailLoginForm.email)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        // 60s 倒计时
                        sendEmailCodeDisable.value = true;
                        sendEmailCodeBtnText.value = "".concat(remainingTime, "s");
                        sendEmailCodeTimer = setInterval(function () {
                            remainingTime--;
                            sendEmailCodeBtnText.value = "".concat(remainingTime, "s");
                            if (remainingTime < 0) {
                                clearInterval(sendEmailCodeTimer);
                                sendEmailCodeDisable.value = false;
                                sendEmailCodeBtnText.value = "发送验证码";
                                remainingTime = 60;
                            }
                        }, 1000);
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "发送验证码");
                });
            }
        });
    }
};
/**
 * 提交邮箱登录表单
 *
 * @param formData 邮箱登录表单
 */
var handleEmailLoginFormSubmit = function (formData) {
    loginLoading.value = true;
    emailLoginSubmit(__assign(__assign({}, formData), { rememberMe: rememberMe.value }))
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            handleLoginResult(data, "email");
        });
    })
        .catch(function (err) {
        handleApiError(err, "登录");
    })
        .finally(function () {
        loginLoading.value = false;
    });
};
/**
 * 处理登录结果
 *
 * @param result 登录结果
 */
var handleLoginResult = function (result, loginType) {
    // 需要修改密码
    if (result.needChangePwd) {
        router.push({
            path: "/login/changePwd",
            query: {
                type: result.changePwdType || "0",
            },
        });
        return;
    }
    if (result.enableMfa) {
        // 进入多因素认证
        toMfa.value = true;
        toBind.value = !result.bound;
        if (result.qrCode) {
            qrCodeData.value = result.qrCode;
        }
    }
    else {
        toTarget();
    }
};
/** 忘记密码 */
var toFogotPwd = ref(false);
var toCheckForgotPwdCode = ref(false);
/**
 * 跳转到忘记密码
 */
var handleToForgotPwd = function () {
    toFogotPwd.value = true;
    toCheckForgotPwdCode.value = true;
    toResetPwd.value = false;
};
/** 检查忘记密码的邮箱验证码 */
var checkForgotPwdCodeFormRef = ref();
var checkForgotPwdCodeForm = reactive({
    username: undefined,
    code: undefined,
});
var checkForgotPwdCodeFormRules = reactive({
    username: [
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
});
var sendForgotPwdEmailCodeDisable = ref(false);
var sendForgotPwdEmailCodeBtnText = ref("发送验证码");
var forgotPwdRemainingTime = 60;
var sendForgotPwdEmailCodeTimer;
/**
 * 发送忘记密码的邮箱验证码
 */
var handleSendForgotPwdEmailCode = function () {
    if (!sendForgotPwdEmailCodeDisable.value) {
        checkForgotPwdCodeFormRef.value.validateField("username", function (err) {
            if (!err) {
                sendEmailCodeSubmit(checkForgotPwdCodeForm.username)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        // 60s 倒计时
                        sendForgotPwdEmailCodeDisable.value = true;
                        sendForgotPwdEmailCodeBtnText.value = "".concat(forgotPwdRemainingTime, "s");
                        sendForgotPwdEmailCodeTimer = setInterval(function () {
                            forgotPwdRemainingTime--;
                            sendForgotPwdEmailCodeBtnText.value = "".concat(forgotPwdRemainingTime, "s");
                            if (forgotPwdRemainingTime < 0) {
                                clearInterval(sendForgotPwdEmailCodeTimer);
                                sendForgotPwdEmailCodeDisable.value = false;
                                sendForgotPwdEmailCodeBtnText.value = "发送验证码";
                                forgotPwdRemainingTime = 60;
                            }
                        }, 1000);
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "发送验证码");
                });
            }
        });
    }
};
/**
 * 提交检查忘记密码的邮箱验证码表单
 */
var handleCheckForgotPwdCodeFormSubmit = function (formData) {
    checkCode(formData)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            resetPwdForm.username = formData.username;
            resetPwdForm.resetPwdToken = data.resultToken;
            checkForgotPwdCodeFormRef.value.resetFields();
            // 跳转重置密码页面
            toCheckForgotPwdCode.value = false;
            toResetPwd.value = true;
        });
    })
        .catch(function (err) {
        handleApiError(err, "校验验证码");
    });
};
/**
 * 返回登录页面
 */
var handleBackupToLogin = function () {
    checkForgotPwdCodeFormRef.value.resetFields();
    toCheckForgotPwdCode.value = false;
    toResetPwd.value = false;
    toFogotPwd.value = false;
};
/** 重置密码 */
var toResetPwd = ref(false);
/** 重置密码表单 */
var resetPwdFormRef = ref();
var resetPwdForm = reactive({
    username: undefined,
    newPwd: undefined,
    confirmPwd: undefined,
    resetPwdToken: undefined,
});
var resetPwdFormRules = {
    newPwd: [{ required: true, message: "新密码未填写" }],
    confirmPwd: [
        { required: true, message: "确认新密码未填写" },
        {
            validator: function (value, cb) {
                if (value !== resetPwdForm.newPwd) {
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
 * 提交重置密码表单
 */
var handleResetPwdFormSubmit = function (formData) {
    if (!checkRes.valid) {
        return;
    }
    delete formData.confirmPwd;
    delete formData.username;
    resetPwd(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("重置密码成功");
            resetPwdFormRef.value.resetFields();
            // 返回登录页
            toFogotPwd.value = false;
            toCheckForgotPwdCode.value = false;
            toResetPwd.value = false;
        });
    })
        .catch(function (err) {
        handleApiError(err, "重置密码");
    });
};
/**
 * 返回忘记密码页面
 */
var handleBackToForgotPwd = function () {
    resetPwdFormRef.value.resetFields();
    toResetPwd.value = false;
    toCheckForgotPwdCode.value = true;
};
/**
 * 密码检查
 */
var checkLoading = ref(false);
var checkRes = reactive({
    valid: false,
    errorMessage: undefined,
    ruleResults: undefined,
});
var handleCheckPassword = function (password) {
    checkLoading.value = true;
    resetPwdForm.newPwd = password;
    checkPasswordWithoutPolicy({
        identity: resetPwdForm.username,
        password: password,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            checkRes.valid = data.valid;
            checkRes.errorMessage = data.errorMessage;
            if (data.ruleResults) {
                checkRes.ruleResults = data.ruleResults;
            }
            else {
                checkRes.ruleResults = [];
            }
            checkLoading.value = false;
        });
    })
        .catch(function (err) {
        handleApiError(err, "密码检查");
        checkLoading.value = false;
    });
};
export default defineComponent({
    components: {
        FederationLogin: FederationLogin
    },
    setup: function () {
        onMounted(function () {
            tenantName.value = localStorage.getItem(TENANT_NAME);
        });
        return {
            tenantName: tenantName,
            passwordLoginForm: passwordLoginForm,
            passwordLoginRules: passwordLoginRules,
            toMfa: toMfa,
            toBind: toBind,
            qrCodeData: qrCodeData,
            totpValidForm: totpValidForm,
            backToLogin: backToLogin,
            captchaVerifyRef: captchaVerifyRef,
            openCaptchaVerify: openCaptchaVerify,
            handlePasswordLoginFromSubmit: handlePasswordLoginFromSubmit,
            handleTotpValidSubmit: handleTotpValidSubmit,
            emailLoginFormRef: emailLoginFormRef,
            emailLoginForm: emailLoginForm,
            emailLoginFormRules: emailLoginFormRules,
            handleSendEmailCode: handleSendEmailCode,
            handleEmailLoginFormSubmit: handleEmailLoginFormSubmit,
            sendEmailCodeDisable: sendEmailCodeDisable,
            sendEmailCodeBtnText: sendEmailCodeBtnText,
            loginLoading: loginLoading,
            toFogotPwd: toFogotPwd,
            toCheckForgotPwdCode: toCheckForgotPwdCode,
            handleToForgotPwd: handleToForgotPwd,
            checkForgotPwdCodeFormRef: checkForgotPwdCodeFormRef,
            checkForgotPwdCodeForm: checkForgotPwdCodeForm,
            checkForgotPwdCodeFormRules: checkForgotPwdCodeFormRules,
            sendForgotPwdEmailCodeDisable: sendForgotPwdEmailCodeDisable,
            sendForgotPwdEmailCodeBtnText: sendForgotPwdEmailCodeBtnText,
            handleSendForgotPwdEmailCode: handleSendForgotPwdEmailCode,
            handleBackupToLogin: handleBackupToLogin,
            handleCheckForgotPwdCodeFormSubmit: handleCheckForgotPwdCodeFormSubmit,
            toResetPwd: toResetPwd,
            resetPwdFormRef: resetPwdFormRef,
            resetPwdForm: resetPwdForm,
            resetPwdFormRules: resetPwdFormRules,
            handleResetPwdFormSubmit: handleResetPwdFormSubmit,
            handleBackToForgotPwd: handleBackToForgotPwd,
            mfaValidLoading: mfaValidLoading,
            checkLoading: checkLoading,
            checkRes: checkRes,
            handleCheckPassword: handleCheckPassword,
            rememberMe: rememberMe
        };
    },
});
