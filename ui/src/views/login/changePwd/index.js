import { changePwd } from "@/api/login";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Notification } from "@arco-design/web-vue";
import { logoutSubmit } from "@/api/logout";
import { checkPasswordWithoutPolicy } from "@/api/setting";
var type = ref("0");
var changePwdForm = reactive({
    rawPwd: "",
    newPwd: "",
    confirmPwd: "",
});
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
 * 提交变更密码表单
 *
 * @param formData 变更密码表单
 */
var handleChangePwdFormSubmit = function (formData) {
    if (!checkRes.valid) {
        return;
    }
    changePwd({
        rawPwd: formData.rawPwd,
        newPwd: formData.newPwd,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("修改密码成功");
            router.push({
                path: "/",
            });
        });
    })
        .catch(function (err) {
        handleApiError(err, "修改密码");
        // 登出
        logoutSubmit()
            .then(function (result) {
            handleApiSuccess(result, function () {
                router.push({
                    path: "/oauth2/redirect",
                });
            });
        })
            .catch(function (err) {
            handleApiError(err, "登出");
        });
    });
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
    changePwdForm.newPwd = password;
    checkPasswordWithoutPolicy({
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
    setup: function () {
        onMounted(function () {
            type.value = getQueryString("type") || "0";
        });
        return {
            type: type,
            changePwdForm: changePwdForm,
            changePwdFormRules: changePwdFormRules,
            handleChangePwdFormSubmit: handleChangePwdFormSubmit,
            checkLoading: checkLoading,
            checkRes: checkRes,
            handleCheckPassword: handleCheckPassword,
        };
    },
});
