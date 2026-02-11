<script lang="ts">
import loginTs from "./index";

export default loginTs;
</script>

<style scoped lang="scss">
@use "./index.scss";
</style>

<template>
  <div class="login-container">
    <div class="form-container" v-if="!toMfa && !toForgotPwd">
      <div v-if="tenantName" class="tenant-name">
        <icon-font
          type="icon-tenant"
          style="margin-right: 8px; font-size: 24px"
        />
        <span>{{ tenantName }}</span>
      </div>
      <div class="title">统一身份认证</div>
      <a-tabs>
        <a-tab-pane key="1" title="密码登录">
          <a-form
            size="large"
            :model="passwordLoginForm"
            :rules="passwordLoginRules"
            @submit-success="openCaptchaVerify"
          >
            <a-form-item field="username" hide-label>
              <a-input
                v-model="passwordLoginForm.username"
                placeholder="请输入手机号 / 邮箱 / 用户名"
              >
                <template #prefix>
                  <icon-user />
                </template>
              </a-input>
            </a-form-item>
            <a-form-item field="password" hide-label>
              <a-input-password
                v-model="passwordLoginForm.password"
                placeholder="请输入登录密码"
              >
                <template #prefix>
                  <icon-lock />
                </template>
              </a-input-password>
            </a-form-item>
            <a-form-item hide-label>
              <a-button
                html-type="submit"
                type="primary"
                class="login-btn"
                :loading="loginLoading"
                >登 录</a-button
              >
            </a-form-item>
          </a-form>
          <div
            style="
              display: flex;
              justify-content: space-between;
              align-items: center;
            "
          >
            <a-checkbox v-model="rememberMe">
              <span style="color: var(--color-text-2)"> 记住我 </span>
            </a-checkbox>
            <a-link @click="handleToForgotPwd">忘记密码</a-link>
          </div>
        </a-tab-pane>
        <a-tab-pane key="2" title="邮箱登录">
          <a-form
            ref="emailLoginFormRef"
            size="large"
            :model="emailLoginForm"
            :rules="emailLoginFormRules"
            @submit-success="handleEmailLoginFormSubmit"
          >
            <a-form-item field="email" hide-label>
              <a-input v-model="emailLoginForm.email" placeholder="请输入邮箱">
                <template #prefix>
                  <icon-user />
                </template>
              </a-input>
            </a-form-item>
            <a-form-item field="code" hide-label>
              <a-row style="width: 100%">
                <a-col :span="16">
                  <a-input
                    v-model="emailLoginForm.code"
                    placeholder="请输入验证码"
                  >
                    <template #prefix>
                      <icon-safe />
                    </template>
                  </a-input>
                </a-col>
                <a-col :span="8">
                  <div class="send-code-btn-container">
                    <a-button
                      style="width: 100%"
                      type="outline"
                      :disabled="sendEmailCodeDisable"
                      @click="handleSendEmailCode"
                      >{{ sendEmailCodeBtnText }}</a-button
                    >
                  </div>
                </a-col>
              </a-row>
            </a-form-item>
            <a-form-item hide-label>
              <a-button html-type="submit" type="primary" class="login-btn"
                >登 录</a-button
              >
            </a-form-item>
          </a-form>
          <a-checkbox v-model="rememberMe" style="height: 24px">
            <span style="color: var(--color-text-2)"> 记住我 </span>
          </a-checkbox>
        </a-tab-pane>
        <a-tab-pane key="3" title="Passkey 登录" v-if="isWebAuthnSupported">
          <a-button
            type="primary"
            class="login-btn"
            style="margin-bottom: 20px; height: 36px"
            :loading="passkeyLoginLoading"
            @click="handlePasskeyLoginSubmit"
          >
            <template #icon>
              <icon-safe />
            </template>
            使用 Passkey 登录
          </a-button>
          <a-checkbox v-model="rememberMe" style="height: 24px">
            <span style="color: var(--color-text-2)"> 记住我 </span>
          </a-checkbox>
        </a-tab-pane>
      </a-tabs>
      <FederationLogin />
    </div>
    <div class="form-container" v-if="toMfa && toMfaValidate && !toForgotPwd">
      <a-spin style="width: 100%; height: 100%" :loading="mfaValidLoading">
        <a-button type="text" size="mini" @click="backToLogin">
          返回登录
          <template #icon>
            <icon-left />
          </template>
        </a-button>
        <div class="title">请选择一种方式进行 MFA 验证</div>
        <a-tabs type="rounded" size="medium" @change="handleMfaMethodChange">
          <a-tab-pane
            style="height: 280px"
            key="passkey"
            title="Passkey"
            v-if="mfaMethods.includes('WEBAUTHN')"
          >
            <div class="mfa-info">请点击下方按钮，进行验证</div>
            <a-button
              type="primary"
              style="width: 100%; margin-top: 20px"
              size="large"
              :loading="webAuthnMfaLoading"
              @click="handleWebAuthnMfa"
            >
              <template #icon>
                <icon-safe />
              </template>
              使用 Passkey 验证
            </a-button>
          </a-tab-pane>
          <a-tab-pane
            style="height: 180px"
            key="totp"
            title="TOTP"
            v-if="mfaMethods.includes('TOTP')"
          >
            <div class="mfa-info">
              输入身份验证应用生成的 6 位验证码，进行验证
            </div>
            <a-verification-code
              ref="totpVerificationCodeRef"
              size="large"
              v-model="totpValidForm.code"
              style="width: 100%; margin-top: 20px; padding: 4px"
              :separator="
                (index) => ((index + 1) % 3 || index > 3 ? null : '-')
              "
              @finish="handleTotpValidSubmit"
            />
          </a-tab-pane>
        </a-tabs>
      </a-spin>
    </div>
    <div class="form-container" v-if="toMfa && !toForgotPwd && !toMfaValidate">
      <div>
        <a-button type="text" size="mini" @click="backToLogin">
          返回登录
          <template #icon>
            <icon-left />
          </template>
        </a-button>
        <div class="title">请选择第二种验证方式开始 MFA 验证</div>
        <a-tabs type="rounded" size="medium">
          <a-tab-pane
            style="height: 280px"
            key="passkey"
            title="Passkey"
            v-if="toAddPasskey"
          >
            <div class="mfa-info">请点击下方按钮，添加 Passkey 凭证</div>
            <div class="mfa-passkey">
              <a-button
                class="mfa-btn"
                type="primary"
                size="large"
                :loading="webAuthnMfaLoading"
                @click="handleAddPasskey"
              >
                <template #icon>
                  <icon-plus />
                </template>
                添加 Passkey 凭证
              </a-button>
            </div>
          </a-tab-pane>
          <a-tab-pane
            style="height: 280px"
            key="totp"
            title="TOTP"
            v-if="toBind"
          >
            <div class="mfa-info">
              请身份验证应用程序（Microsoft Authenticator
              等）扫描以下二维码，添加 TOTP 验证方式
            </div>
            <div class="mfa-code">
              <div>
                <img :src="qrCodeData" />
              </div>
              <a-button
                type="primary"
                class="mfa-btn"
                @click="handleToValidateTotp"
                >下一步</a-button
              >
            </div>
          </a-tab-pane>
        </a-tabs>
      </div>
    </div>
    <div class="form-container" v-if="toForgotPwd">
      <div class="forgot-pwd-container" v-if="toCheckForgotPwdCode">
        <div class="forgot-pwd-title">重置密码</div>
        <div class="forgot-pwd-info">
          请输入你注册的邮箱用于接收验证码，将为你重置密码。
        </div>
        <a-form
          ref="checkForgotPwdCodeFormRef"
          size="large"
          :model="checkForgotPwdCodeForm"
          :rules="checkForgotPwdCodeFormRules"
          @submit-success="handleCheckForgotPwdCodeFormSubmit"
        >
          <a-form-item field="username" hide-label>
            <a-input
              v-model="checkForgotPwdCodeForm.username"
              placeholder="请输入邮箱"
            >
              <template #prefix>
                <icon-user />
              </template>
            </a-input>
          </a-form-item>
          <a-form-item field="code" hide-label>
            <a-row style="width: 100%">
              <a-col :span="16">
                <a-input
                  v-model="checkForgotPwdCodeForm.code"
                  placeholder="请输入验证码"
                >
                  <template #prefix>
                    <icon-safe />
                  </template>
                </a-input>
              </a-col>
              <a-col :span="8">
                <div class="send-code-btn-container">
                  <a-button
                    style="width: 100%"
                    type="outline"
                    :disabled="sendForgotPwdEmailCodeDisable"
                    @click="handleSendForgotPwdEmailCode"
                    >{{ sendForgotPwdEmailCodeBtnText }}</a-button
                  >
                </div>
              </a-col>
            </a-row>
          </a-form-item>
          <a-form-item hide-label>
            <a-button html-type="submit" type="primary" class="login-btn"
              >下一步</a-button
            >
          </a-form-item>
        </a-form>
        <div class="backup">
          <a-link size="mini" @click="handleBackupToLogin">返回</a-link>
        </div>
      </div>
      <div class="forgot-pwd-container" v-if="toResetPwd">
        <div class="forgot-pwd-title">重置密码</div>
        <div class="forgot-pwd-info">
          {{ `你正在重置 ${resetPwdForm.username} 的密码。` }}
        </div>
        <a-form
          ref="resetPwdFormRef"
          size="large"
          :model="resetPwdForm"
          :rules="resetPwdFormRules"
          @submit-success="handleResetPwdFormSubmit"
        >
          <a-form-item field="newPwd" label="新密码" hide-label>
            <password-checker
              type="password"
              placeholder="请输入新密码"
              :loading="checkLoading"
              @check="handleCheckPassword"
              :checkRes="checkRes"
            />
          </a-form-item>
          <a-form-item field="confirmPwd" label="确认密码" hide-label>
            <a-input-password
              v-model="resetPwdForm.confirmPwd"
              placeholder="请确认密码"
            />
          </a-form-item>
          <a-form-item hide-label>
            <a-button html-type="submit" type="primary" class="login-btn"
              >重置密码</a-button
            >
          </a-form-item>
        </a-form>
        <div class="backup">
          <a-link size="mini" @click="handleBackToForgotPwd">返回</a-link>
        </div>
      </div>
    </div>
  </div>

  <verify
    @success="handlePasswordLoginFromSubmit"
    mode="pop"
    captchaType="blockPuzzle"
    :imgSize="{ width: '330px', height: '155px' }"
    ref="captchaVerifyRef"
  />
</template>
