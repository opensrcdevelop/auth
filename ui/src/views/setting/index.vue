<script lang="ts">
import settingTs from "./index";

export default settingTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="setting-header">
      <div class="left">
        <div class="title">系统设置</div>
        <div class="info">自定义系统设置，让系统更符合您的使用习惯。</div>
      </div>
    </div>

    <div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="mail_template" title="邮件模版">
          <div class="tab-container">
            <div class="info-title">模版列表</div>
            <div
              class="template-container"
              v-for="item in mailTemplateList"
              :key="item.id"
            >
              <div class="left">
                <div class="icon">
                  <svg
                    t="1742632535021"
                    class="icon"
                    viewBox="0 0 1024 1024"
                    version="1.1"
                    xmlns="http://www.w3.org/2000/svg"
                    p-id="10559"
                    width="64"
                    height="64"
                  >
                    <path
                      d="M85.333333 224A138.666667 138.666667 0 0 1 224 85.333333h448A138.666667 138.666667 0 0 1 810.666667 224v202.965333a184.106667 184.106667 0 0 0-10.666667-0.298666H746.666667V224a74.666667 74.666667 0 0 0-74.666667-74.666667H224A74.666667 74.666667 0 0 0 149.333333 224v490.666667c0 41.216 33.450667 74.666667 74.666667 74.666666H256v10.666667c0 18.56 2.773333 36.48 7.978667 53.333333H224A138.666667 138.666667 0 0 1 85.333333 714.666667V224zM288 213.333333a32 32 0 0 0 0 64h320a32 32 0 0 0 0-64h-320zM341.333333 352a32 32 0 0 1 32-32h234.666667a32 32 0 0 1 0 64h-234.666667A32 32 0 0 1 341.333333 352z m-42.666666 256A138.666667 138.666667 0 0 1 437.333333 469.333333h362.666667A138.666667 138.666667 0 0 1 938.666667 608v192A138.666667 138.666667 0 0 1 800 938.666667h-362.666667A138.666667 138.666667 0 0 1 298.666667 800v-192z m138.666666-74.666667c-21.504 0-40.917333 9.088-54.528 23.68l235.861334 151.594667 235.861333-151.594667a74.453333 74.453333 0 0 0-54.528-23.68h-362.666667zM362.666667 800c0 41.216 33.450667 74.666667 74.666666 74.666667h362.666667a74.666667 74.666667 0 0 0 74.666667-74.666667v-179.882667l-238.677334 153.472a32 32 0 0 1-34.645333 0L362.666667 620.117333v179.882667z"
                      fill="white"
                      p-id="10560"
                    ></path>
                  </svg>
                </div>
                <div class="template-info">
                  <div class="template-name">
                    {{ item.name }}
                  </div>
                  <div class="template-desc">
                    {{ item.desc }}
                  </div>
                </div>
              </div>
              <div>
                <a-button type="text" @click="handleToEditMailTemplate(item.id)"
                  >编辑并预览</a-button
                >
              </div>
            </div>
          </div>
        </a-tab-pane>
        <a-tab-pane key="mail_service" title="邮件服务">
          <div class="tab-container">
            <div class="info-title">服务配置</div>
            <a-form
              :model="mailServiceConfigForm"
              layout="vertical"
              ref="mailServiceConfigFormRef"
              :rules="mailServiceConfigFormRules"
              @submit-success="handleMailServiceConfigFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item label="SMTP 地址" field="host">
                    <a-input
                      placeholder="请输入 SMTP 地址"
                      v-model="mailServiceConfigForm.host"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item label="端口号" field="port">
                    <a-input
                      placeholder="请输入端口号"
                      v-model="mailServiceConfigForm.port"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item label="用户名" field="username">
                    <a-input
                      placeholder="请输入用户名"
                      v-model="mailServiceConfigForm.username"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item label="密码" field="password">
                    <a-input-password
                      placeholder="请输入密码"
                      v-model="mailServiceConfigForm.password"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item label="启用 SSL" field="sslEnable">
                    <a-switch
                      type="round"
                      v-model="mailServiceConfigForm.sslEnable"
                    />
                  </a-form-item>
                </a-col>
              </a-row>

              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetMailServiceConfigForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
            <div class="info-title">消息配置</div>
            <a-form
              :model="mailMessageConfigForm"
              layout="vertical"
              ref="mailMessageConfigFormRef"
              :rules="mailMessageConfigFormRules"
              @submit-success="handleMailMessageConfigFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item label="验证码有效期" field="codeLive">
                    <a-input-number
                      :min="1"
                      :step="1"
                      placeholder="请输入验证码有效期"
                      v-model="mailMessageConfigForm.codeLive"
                    >
                      <template #suffix> 分钟 </template>
                    </a-input-number>
                  </a-form-item>
                </a-col>
              </a-row>

              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetMailMessageConfigForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
      </a-tabs>
    </div>

    <a-drawer
      width="100%"
      :visible="editMailTemplateDrawerVisible"
      :mask="false"
      :footer="false"
      @cancel="() => (editMailTemplateDrawerVisible = false)"
      unmountOnClose
    >
      <template #title>{{ mailTemplateDetailForm.name }}</template>
      <a-spin
        style="width: 100%"
        tip="处理中，请稍后..."
        :loading="globalVariables.apiLoading"
      >
        <div class="drawer-container">
          <div class="edit-container">
            <a-form
              :model="mailTemplateDetailForm"
              layout="vertical"
              :rules="mailTemplateDetailFormRules"
              ref="mailTemplateDetailFormRef"
              @submit-success="handleMailTemplateDetailFormSubmit"
            >
              <a-form-item label="参数">
                <a-descriptions :column="1" bordered style="width: 100%">
                  <a-descriptions-item
                    v-for="item of mailTemplateDetailForm.parameters"
                    :label="item.key"
                  >
                    {{ item.value }}
                  </a-descriptions-item>
                </a-descriptions>
              </a-form-item>
              <a-form-item label="主题" field="subject">
                <a-input
                  placeholder="请输入主题"
                  v-model="mailTemplateDetailForm.subject"
                />
              </a-form-item>
              <a-form-item label="发件人" field="sender">
                <a-input
                  placeholder="请输入发件人"
                  v-model="mailTemplateDetailForm.sender"
                />
              </a-form-item>
              <a-form-item label="模版" field="content">
                <monaco-editor
                  v-model="mailTemplateDetailForm.content"
                  language="html"
                  height="320px"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetMailTemplateDetailForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
          <div class="preview-container">
            <iframe
              :srcdoc="mailTemplateDetailForm.content"
              frameborder="0"
              width="100%"
              height="100%"
            >
            </iframe>
          </div>
        </div>
      </a-spin>
    </a-drawer>
  </div>
</template>
