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
import { getJwtSecretInfo, getJwtSecretRotationConfig, rotateJwtSecret, saveJwtSecretRotationConfig, } from "@/api/setting";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
var activeTab = ref("jwt_secret");
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
        case "jwt_secret":
            handleGetSecretInfo();
            handleGetRotationConfig();
            break;
    }
};
/**
 * 密钥信息
 */
var secretInfo = reactive({
    kid: undefined,
    alg: undefined,
    createTime: undefined,
    expireTime: undefined,
});
var handleGetSecretInfo = function () {
    getJwtSecretInfo()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            secretInfo.kid = data.kid;
            secretInfo.alg = data.alg;
            secretInfo.createTime = data.createTime;
            secretInfo.expireTime = data.expireTime;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取 JWT 密钥信息");
    });
};
/**
 * 密钥轮换配置
 */
var rotationConfigForm = reactive({
    rotationPeriod: undefined,
    rotationPeriodUnit: undefined,
});
var rotationConfigFormRef = ref(null);
var rotationConfigFormRules = {
    rotationPeriod: [{ required: true, message: "轮换周期未填写" }],
    rotationPeriodUnit: [{ required: true, message: "轮换周期单位未选择" }],
};
var handleGetRotationConfig = function () {
    getJwtSecretRotationConfig()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            rotationConfigForm.rotationPeriod = data.rotationPeriod;
            rotationConfigForm.rotationPeriodUnit = data.rotationPeriodUnit;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取 JWT 密钥轮换配置");
    });
};
var handleRotationConfigFormSubmit = function () {
    saveJwtSecretRotationConfig(rotationConfigForm)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            Notification.success("保存成功");
        });
    })
        .catch(function (err) {
        handleApiError(err, "保存 JWT 密钥轮换配置");
    });
};
var handleResetRotationConfigForm = function () {
    rotationConfigFormRef.value.resetFields();
    handleGetRotationConfig();
};
var handleRotateSecret = function () {
    Modal.warning({
        title: "\u786E\u5B9A\u7ACB\u5373\u8F6E\u6362\u5BC6\u94A5\uFF1F",
        content: "轮换后，所有使用旧密钥签发的 Token 将失效，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "warning",
        },
        onOk: function () {
            rotateJwtSecret()
                .then(function (result) {
                Notification.success("密钥轮换成功");
                handleGetSecretInfo();
            })
                .catch(function (err) {
                handleApiError(err, "密钥轮换");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        var tab = getQueryString("active_tab");
        onMounted(function () {
            activeTab.value = tab || "jwt_secret";
            handleTabInit(activeTab.value);
        });
        return {
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            secretInfo: secretInfo,
            rotationConfigForm: rotationConfigForm,
            rotationConfigFormRef: rotationConfigFormRef,
            rotationConfigFormRules: rotationConfigFormRules,
            handleRotationConfigFormSubmit: handleRotationConfigFormSubmit,
            handleResetRotationConfigForm: handleResetRotationConfigForm,
            handleRotateSecret: handleRotateSecret
        };
    },
});
