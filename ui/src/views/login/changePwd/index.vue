<script lang="ts">
import changePwdTs from "./index";

export default changePwdTs;
</script>

<style scoped lang="scss">
@use "./index.scss";
</style>

<template>
  <div class="change-pwd-container">
    <div class="form-container">
      <div class="tip-info-container">
        <div class="change-pwd-title">修改密码</div>
        <div class="change-pwd-info" v-if="type === '0'">
          请修改您的账户密码，以继续使用。
        </div>
        <div class="change-pwd-info" v-if="type === '1'">
          您的账户密码不满足管理员设置的密码策略要求，请修改您的账户密码，以继续使用。
        </div>
      </div>
      <a-form
        :model="changePwdForm"
        layout="vertical"
        :rules="changePwdFormRules"
        @submit-success="handleChangePwdFormSubmit"
      >
        <a-form-item field="rawPwd" label="原密码">
          <a-input-password
            v-model="changePwdForm.rawPwd"
            placeholder="请输入原密码"
          />
        </a-form-item>
        <a-form-item field="newPwd" label="新密码">
          <password-checker
            type="password"
            placeholder="请输入新密码"
            :loading="checkLoading"
            @check="handleCheckPassword"
            :checkRes="checkRes"
          />
        </a-form-item>
        <a-form-item field="confirmPwd" label="确认密码">
          <a-input-password
            v-model="changePwdForm.confirmPwd"
            placeholder="请确认密码"
          />
        </a-form-item>
        <a-form-item hide-label>
          <a-button html-type="submit" type="primary" class="submit-btn"
            >修改密码</a-button
          >
        </a-form-item>
      </a-form>
    </div>
  </div>
</template>
