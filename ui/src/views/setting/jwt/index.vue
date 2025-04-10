<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="setting-header">
      <div class="left">
        <div class="title">JWT 设置</div>
        <div class="info">支持自定义密钥轮换设置。</div>
      </div>
    </div>

    <a-tabs :active-key="activeTab" @change="handleTabChange">
      <a-tab-pane key="jwt_secret" title="JWT 密钥">
        <div class="tab-container">
          <div class="info-title">当前密钥</div>
          <a-descriptions bordered :column="2">
            <a-descriptions-item label="ID">{{
              secretInfo.kid
            }}</a-descriptions-item>
            <a-descriptions-item label="签名算法">{{
              secretInfo.alg
            }}</a-descriptions-item>
            <a-descriptions-item label="创建时间">{{
              secretInfo.createTime
            }}</a-descriptions-item>
            <a-descriptions-item label="过期时间">{{
              secretInfo.expireTime
            }}</a-descriptions-item>
          </a-descriptions>
          <a-button
            type="primary"
            style="margin: 16px 0 16px 0"
            @click="handleRotateSecret"
            >立即轮换</a-button
          >
          <div class="info-title">轮换配置</div>
          <a-form
            :model="rotationConfigForm"
            ref="rotationConfigFormRef"
            :rules="rotationConfigFormRules"
            layout="vertical"
            @submit-success="handleRotationConfigFormSubmit"
          >
            <a-row :gutter="24">
              <a-col :span="12">
                <a-form-item label="轮换周期" field="rotationPeriod">
                  <a-input-number
                    v-model="rotationConfigForm.rotationPeriod"
                    :min="1"
                    :max="99"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item label="轮换周期单位" field="rotationPeriodUnit">
                  <a-select v-model="rotationConfigForm.rotationPeriodUnit">
                    <a-option value="DAY">天</a-option>
                    <a-option value="MONTH">月</a-option>
                    <a-option value="YEAR">年</a-option>
                  </a-select>
                </a-form-item>
              </a-col>
            </a-row>
            <a-form-item hide-label>
              <a-space>
                <a-button type="primary" html-type="submit">保存</a-button>
                <a-button @click="handleResetRotationConfigForm">重置</a-button>
              </a-space>
            </a-form-item>
          </a-form>
        </div>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
