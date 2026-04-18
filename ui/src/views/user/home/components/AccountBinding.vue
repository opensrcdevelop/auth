<script lang="ts" setup>
import {bindUser, getBoundIdentitySource, unbindUser,} from "@/api/identitySource";
import {bindEmail, sendBindEmailCode, unbindEmail} from "@/api/user";
import {sendEmailCodeSubmit} from "@/api/login";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {reactive, ref, watch} from "vue";
import {AUTH_FAILURE, AUTH_SUCCESS, BINDING_EXISTS,} from "@/util/constants";
import webauthn from "@/util/webauthn";
import {
  completeWebAuthnRegistration,
  deleteWebAuthnCredential,
  getWebAuthnRegisterOptions,
  listWebAuthnCredentials,
} from "@/api/webauthn";

const props = defineProps<{
  userInfo: Record<string, any>;
  activeKey?: string;
}>();

/** 绑定的身份源 */
const boundIdentitySource = reactive<any[]>([]);

/**
 * 获取绑定的身份源
 */
const handleGetBoundIdentitySource = () => {
  getBoundIdentitySource()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        boundIdentitySource.length = 0;
        boundIdentitySource.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取绑定的身份源");
    });
};

/** WebAuthn 凭证列表 */
const webAuthnCredentials = reactive<any[]>([]);

/** 是否正在添加凭证 */
const addingWebAuthnCredential = ref(false);

/**
 * 获取 WebAuthn 凭证列表
 */
const handleGetWebAuthnCredentials = () => {
  listWebAuthnCredentials()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        webAuthnCredentials.length = 0;
        webAuthnCredentials.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取 WebAuthn 凭证");
    });
};

/**
 * 添加 WebAuthn 凭证
 */
const handleAddWebAuthnCredential = () => {
  // 检查浏览器是否支持 WebAuthn
  if (!webauthn.isSupported()) {
    Notification.warning("当前浏览器不支持 Passkey");
    return;
  }

  addingWebAuthnCredential.value = true;
  getWebAuthnRegisterOptions()
    .then((result: any) => {
      handleApiSuccess(result, async (data: any) => {
        try {
          const credential = await webauthn.startRegistration(data);
          await completeWebAuthnRegistration({
            id: credential.id,
            rawId: credential.rawId,
            response: {
              clientDataJSON: credential.response.clientDataJSON,
              attestationObject: credential.response.attestationObject,
            },
            transports: credential.response.transports?.join(",") || "",
          });
          Notification.success("添加 Passkey 凭证成功");
          handleGetWebAuthnCredentials();
        } catch (error: any) {
          if (error.message && error.message.includes("not allowed")) {
            Notification.warning("已取消添加 Passkey 凭证");
          } else if (error.message.includes("previously registered")) {
            Notification.warning("该设备已注册过 Passkey，无法重复注册");
          } else {
            Notification.error(
              "添加凭证失败: " + (error.message || "未知错误"),
            );
          }
        }
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取注册选项");
    })
    .finally(() => {
      addingWebAuthnCredential.value = false;
    });
};

/**
 * 删除 WebAuthn 凭证
 */
const handleDeleteWebAuthnCredential = (credential: any) => {
  deleteWebAuthnCredential(credential.id)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("删除凭证成功");
        handleGetWebAuthnCredentials();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "删除凭证");
    });
};

/** 绑定 / 解绑邮箱对话框 */
const bindOrUnbindEmailModalVisible = ref(false);
const isBinding = ref(true);
const bindOrUnbindEmailFormSubmitLoading = ref(false);
const bindOrUnbindEmailFormRef = ref();
const bindOrUnbindEmailForm = reactive({
  email: undefined as string | undefined,
  code: undefined as string | undefined,
});
const bindOrUnbindEmailFormRules = {
  email: [{ required: true, message: "邮箱未填写" }],
  code: [{ required: true, message: "验证码未填写" }],
};

/** 发送邮箱验证码 */
const sendEmailCodeDisable = ref(false);
const sendEmailCodeBtnText = ref("发送验证码");
let remainingTime = 60;
let sendEmailCodeTimer: ReturnType<typeof setInterval>;

/**
 * 打开绑定邮箱对话框
 */
const handleOpenBindEmailModal = () => {
  isBinding.value = true;
  bindOrUnbindEmailModalVisible.value = true;
};

/**
 * 打开解绑邮箱对话框
 */
const handleOpenUnbindEmailModal = () => {
  isBinding.value = false;
  bindOrUnbindEmailForm.email = props.userInfo["emailAddress"];
  bindOrUnbindEmailModalVisible.value = true;
};

/**
 * 关闭绑定 / 解绑邮箱对话框
 */
const handleCoseBindOrUnbindEmailModal = () => {
  bindOrUnbindEmailModalVisible.value = false;
  bindOrUnbindEmailFormRef.value.resetFields();
};

/**
 * 提交绑定 / 解绑邮箱表单
 */
const handleBindOrUnbindEmailFormSubmit = () => {
  bindOrUnbindEmailFormRef.value.validate(async (err: any) => {
    if (!err) {
      try {
        bindOrUnbindEmailFormSubmitLoading.value = true;
        if (isBinding.value) {
          await bindEmail(bindOrUnbindEmailForm);
          Notification.success("绑定邮箱成功");
        } else {
          await unbindEmail(bindOrUnbindEmailForm);
          Notification.success("解绑邮箱成功");
        }
        handleCoseBindOrUnbindEmailModal();
        handleGetBoundIdentitySource();
      } catch (err: any) {
        handleApiError(err, "绑定 / 解绑邮箱");
      } finally {
        bindOrUnbindEmailFormSubmitLoading.value = false;
      }
    }
  });
};

/**
 * 发送邮箱验证码
 */
const handleSendEmailCode = () => {
  if (!sendEmailCodeDisable.value) {
    bindOrUnbindEmailFormRef.value.validateField("email", async (err: any) => {
      if (!err) {
        try {
          if (isBinding.value) {
            await sendBindEmailCode(bindOrUnbindEmailForm.email);
          } else {
            await sendEmailCodeSubmit(bindOrUnbindEmailForm.email);
          }
          // 60s 倒计时
          sendEmailCodeDisable.value = true;
          sendEmailCodeBtnText.value = `${remainingTime}s 后重试`;
          sendEmailCodeTimer = setInterval(() => {
            remainingTime--;
            sendEmailCodeBtnText.value = `${remainingTime}s 后重试`;
            if (remainingTime < 0) {
              clearInterval(sendEmailCodeTimer);
              sendEmailCodeDisable.value = false;
              sendEmailCodeBtnText.value = "发送验证码";
              remainingTime = 60;
            }
          }, 1000);
        } catch (err) {
          handleApiError(err, "发送验证码");
        }
      }
    });
  }
};

/**
 * 绑定第三方账号
 */
let authWindow: Window | null;
const handleBindUser = (identitySource: any) => {
  bindUser(identitySource.code)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        // 打开第三方认证窗口
        authWindow = window.open(
          data.authReqUri,
          "_blank",
          "width=600,height=600",
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "绑定第三方账号");
    });
};

/**
 * 处理第三方认证窗口响应
 */
const handleAuthWindowResponse = (event: any) => {
  if (event.data === AUTH_SUCCESS) {
    Notification.success("第三方账号绑定成功");
    authWindow?.close();
    handleGetBoundIdentitySource();
  }

  if (event.data === AUTH_FAILURE) {
    Notification.error("第三方账号绑定失败");
    authWindow?.close();
  }

  if (event.data === BINDING_EXISTS) {
    Notification.error("该第三方账号已绑定其他用户，请先解绑");
    authWindow?.close();
  }
};

/**
 * 解绑第三方账号
 */
const handleUnbindUser = (identitySource: any) => {
  Modal.warning({
    title: `确定与「${identitySource.name}」解除绑定吗？`,
    content: "",
    okButtonProps: {
      status: "warning",
    },
    onOk: () => {
      unbindUser(identitySource.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("解绑成功");
            handleGetBoundIdentitySource();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "解绑第三方账号");
        });
    },
  });
};

// 监听第三方认证窗口响应
window.addEventListener("message", handleAuthWindowResponse);

/**
 * 执行初始化
 */
const handleInit = () => {
  handleGetBoundIdentitySource();
  handleGetWebAuthnCredentials();
};

/**
 * 监听 tab 切换，当激活当前 tab 时才执行初始化
 */
watch(
  () => props.activeKey,
  (newActiveKey) => {
    if (newActiveKey === "account_binding") {
      handleInit();
    }
  },
  { immediate: true },
);
</script>

<template>
  <div class="card">
    <a-card title="手机号和邮箱">
      <div class="binding-card">
        <div class="icon-container">
          <div class="icon">
            <icon-email />
          </div>
          <span>邮箱</span>
          <span
            v-if="userInfo['emailAddress']"
            style="color: #396aff; margin-left: 8px"
            >{{ userInfo["emailAddress"] }}</span
          >
        </div>
        <div class="status-container">
          <div class="binding" v-if="!userInfo['emailAddress']">
            <a-button type="text" @click="handleOpenBindEmailModal">
              <template #icon>
                <icon-font type="icon-binding" />
              </template>
              绑定
            </a-button>
          </div>
          <div class="unbind" v-else>
            <a-button
              type="text"
              status="warning"
              @click="handleOpenUnbindEmailModal"
            >
              <template #icon>
                <icon-font type="icon-unbind" />
              </template>
              解除绑定
            </a-button>
          </div>
        </div>
      </div>
    </a-card>
  </div>
  <div class="card" v-if="boundIdentitySource.length > 0">
    <a-card title="第三方账号">
      <div
        class="binding-card"
        v-for="identitySource in boundIdentitySource"
        :key="identitySource.id"
      >
        <div class="icon-container">
          <div class="icon">
            <img
              class="identity-source-logo"
              :src="identitySource.logo"
              :draggable="false"
            />
          </div>
          <div class="name-container">
            <span>{{ identitySource.name }}</span>
            <span class="username" v-if="identitySource.bindUsername"
              >用户名：{{ identitySource.bindUsername }}</span
            >
          </div>
        </div>
        <div class="status-container">
          <div class="binding" v-if="!identitySource.isBind">
            <a-button type="text" @click="handleBindUser(identitySource)">
              <template #icon>
                <icon-font type="icon-binding" />
              </template>
              绑定
            </a-button>
          </div>
          <div class="unbind" v-else>
            <a-button
              type="text"
              status="warning"
              @click="handleUnbindUser(identitySource)"
            >
              <template #icon>
                <icon-font type="icon-unbind" />
              </template>
              解除绑定
            </a-button>
          </div>
        </div>
      </div>
    </a-card>
  </div>
  <div class="card">
    <a-card title="Passkey 凭证">
      <template #extra>
        <a-button
          type="text"
          @click="handleAddWebAuthnCredential"
          :loading="addingWebAuthnCredential"
        >
          <template #icon>
            <icon-plus />
          </template>
          添加凭证
        </a-button>
      </template>
      <a-table
        :data="webAuthnCredentials"
        :bordered="false"
        :pagination="false"
      >
        <template #columns>
          <a-table-column title="凭证 ID" ellipsis tooltip>
            <template #cell="{ record }">
              {{ record.id }}
            </template>
          </a-table-column>
          <a-table-column title="设备类型">
            <template #cell="{ record }">
              <a-tag v-if="record.deviceType === 'platform'" color="arcoblue">
                平台设备
              </a-tag>
              <a-tag
                v-else-if="record.deviceType === 'cross-platform'"
                color="green"
              >
                跨平台设备
              </a-tag>
              <a-tag v-else>{{ record.deviceType }}</a-tag>
            </template>
          </a-table-column>
          <a-table-column title="创建时间">
            <template #cell="{ record }">
              {{ record.createdAt ? record.createdAt : "-" }}
            </template>
          </a-table-column>
          <a-table-column title="最后使用">
            <template #cell="{ record }">
              {{ record.lastUsedAt ? record.lastUsedAt : "-" }}
            </template>
          </a-table-column>
          <a-table-column title="操作" :width="80">
            <template #cell="{ record }">
              <a-popconfirm
                type="warning"
                content="确定删除此凭证吗？删除后无法使用该设备进行认证。"
                :ok-button-props="{ status: 'danger' }"
                @ok="handleDeleteWebAuthnCredential(record)"
              >
                <a-button status="danger" size="small"> 删除 </a-button>
              </a-popconfirm>
            </template>
          </a-table-column>
        </template>
      </a-table>
    </a-card>
  </div>

  <!-- 绑定 / 解绑邮箱对话框 -->
  <a-modal
    :visible="bindOrUnbindEmailModalVisible"
    @cancel="handleCoseBindOrUnbindEmailModal"
    @ok="handleBindOrUnbindEmailFormSubmit"
    :ok-loading="bindOrUnbindEmailFormSubmitLoading"
  >
    <template #title>{{ isBinding ? "绑定邮箱" : "解绑邮箱" }}</template>
    <a-form
      :model="bindOrUnbindEmailForm"
      :rules="bindOrUnbindEmailFormRules"
      ref="bindOrUnbindEmailFormRef"
      layout="vertical"
    >
      <a-form-item field="email" label="邮箱">
        <a-input
          v-model="bindOrUnbindEmailForm.email"
          :readonly="!isBinding"
          placeholder="请输入邮箱"
        />
      </a-form-item>
      <a-form-item field="code" label="验证码">
        <a-input-group style="width: 100%">
          <a-input v-model="bindOrUnbindEmailForm.code" placeholder="请输入验证码" />
          <a-button
            type="primary"
            :disabled="sendEmailCodeDisable"
            @click="handleSendEmailCode"
            >{{ sendEmailCodeBtnText }}</a-button
          >
        </a-input-group>
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<style lang="scss" scoped>
.card {
  margin-bottom: 16px;
}

.binding-card {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
  height: 40px;
  margin-bottom: 16px;

  .icon-container {
    display: flex;
    align-items: center;
    font-size: 16px;

    .icon {
      height: 36px;
      width: 36px;
      border-radius: 8px;
      display: flex;
      align-items: center;
      justify-content: center;
      background-color: #eff2f6;
      font-size: 20px;
      margin-right: 8px;
    }

    .identity-source-logo {
      width: 36px;
      height: 36px;
      object-fit: contain;
      border-radius: 8px;
    }

    .name-container {
      display: flex;
      flex-direction: column;

      .username {
        margin-top: 4px;
        color: #86909c;
        font-size: 14px;
      }
    }
  }

  .status-container {
    display: flex;
    align-items: center;
  }
}

.binding-card:last-child {
  margin-bottom: 0;
}
</style>
