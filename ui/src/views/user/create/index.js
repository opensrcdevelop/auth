import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { generateRandomString, handleApiError, handleApiSuccess, } from "@/util/tool";
import { createUser } from "@/api/user";
import { Notification } from "@arco-design/web-vue";
import { checkPasswordWithoutPolicy } from "@/api/setting";
/**
 * 返回上一级
 */
var handleBack = function () {
    handleResetCreateUserForm();
    router.back();
};
/** 创建用户表单 */
var createUserForm = reactive({
    username: null,
    password: null,
    phoneNumber: null,
    emailAddress: null,
    needChangePwd: false,
    sendEmail: false,
});
var createUserFormRef = ref();
var createUserFormRules = {
    username: [
        {
            required: true,
            message: "用户名未填写",
        },
    ],
    password: [
        {
            required: true,
            message: "密码未填写",
        },
    ],
    emailAddress: [
        {
            validator: function (value, cb) {
                if (createUserForm.sendEmail && !value) {
                    cb("邮箱未填写");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
/**
 * 生成随机密码
 */
var handleGeneratePassword = function () {
    passwordCheckerRef.value.setPassword(generateRandomString(12));
};
/**
 * 提交创建用户表单
 */
var handleCreateUserFormSubmit = function () {
    if (!checkPasswordRes.valid) {
        return;
    }
    createUser(createUserForm)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateUserForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建用户");
    });
};
/**
 * 重置创建用户表单
 */
var handleResetCreateUserForm = function () {
    createUserFormRef.value.resetFields();
    passwordCheckerRef.value.setPassword("");
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
    createUserForm.password = password;
    checkPasswordWithoutPolicy({
        identity: createUserForm.username,
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
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            createUserForm: createUserForm,
            createUserFormRules: createUserFormRules,
            createUserFormRef: createUserFormRef,
            handleGeneratePassword: handleGeneratePassword,
            handleCreateUserFormSubmit: handleCreateUserFormSubmit,
            handleResetCreateUserForm: handleResetCreateUserForm,
            passwordCheckerRef: passwordCheckerRef,
            checkPasswordLoading: checkPasswordLoading,
            checkPasswordRes: checkPasswordRes,
            handleCheckPassword: handleCheckPassword,
        };
    },
});
