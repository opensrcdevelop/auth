<script lang="ts">
import changePwdTs from "./index";
export default changePwdTs;
</script>

<style scoped lang="scss">
@import "./index.scss";
</style>

<template>
  <div class="change-pwd-container">
    <div class="form-container">
      <div class="tip-info-container">
        <a-alert center>请修改密码。</a-alert>
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
          <a-input-password
            v-model="changePwdForm.newPwd"
            placeholder="请输入新密码"
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
