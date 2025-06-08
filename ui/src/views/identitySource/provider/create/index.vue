<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建身份源提供商</div>
      <div class="info-title">身份源提供商信息</div>
      <a-form
        :model="createProviderForm"
        ref="createProviderFormRef"
        :rules="createProviderFormRules"
        layout="vertical"
        @submit-success="handleCreateProviderFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="name" label="身份源提供商名称">
              <a-input
                v-model="createProviderForm.name"
                placeholder="请输入身份源提供商名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="code" label="身份源提供商标识">
              <a-input
                v-model="createProviderForm.code"
                placeholder="请输入身份源提供商标识"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item field="logo" label="身份源提供商 Logo 地址">
          <a-input
            v-model="createProviderForm.logo"
            placeholder="身份源提供商 Logo 地址"
          />
        </a-form-item>
        <a-form-item field="desc" label="身份源提供商描述">
          <a-textarea
            v-model="createProviderForm.desc"
            placeholder="请输入身份源提供商描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
        <a-form-item field="authorizationUri" label="授权地址">
          <a-input
            v-model="createProviderForm.authorizationUri"
            placeholder="请输入授权地址"
          />
        </a-form-item>
        <a-form-item field="enableCustomAuthzReq" label="自定义授权请求">
          <a-switch
            v-model="createProviderForm.enableCustomAuthzReq"
            type="round"
          />
        </a-form-item>
        <a-form-item
          v-if="createProviderForm.enableCustomAuthzReq"
          field="authzReqCfg"
          label="授权请求配置"
        >
          <monaco-editor
            v-model="createProviderForm.authzReqCfg"
            language="json"
            :editorOption="{
              contextmenu: false,
            }"
            height="220px"
          />
          <template #extra>
            <a-collapse style="width: 100%">
              <a-collapse-item header="配置说明" key="1">
                <a-space direction="vertical" style="width: 100%">
                  <a-descriptions title="JSON 配置项" bordered :column="1">
                    <a-descriptions-item label="params">
                      必填项，类型为对象（Object），对应授权请求地址的 query
                      参数，值支持从上下文中获取，获取方式为 ${XXX}，其中 XXX
                      为上下文变量名。
                    </a-descriptions-item>
                  </a-descriptions>
                  <a-descriptions title="可用的上下文变量" bordered :column="1">
                    <a-descriptions-item label="response_type">
                      响应类型，固定值为 code。
                    </a-descriptions-item>
                    <a-descriptions-item label="client_id">
                      身份源中的 Client ID。
                    </a-descriptions-item>
                    <a-descriptions-item label="scope">
                      授权范围，多个 scope 之间使用空格分隔。
                    </a-descriptions-item>
                    <a-descriptions-item label="state">
                      随机字符串，用于防止 CSRF 攻击。
                    </a-descriptions-item>
                    <a-descriptions-item label="redirect_uri">
                      授权回调地址。
                    </a-descriptions-item>
                    <a-descriptions-item label="其他">
                      身份源中的额外参数。
                    </a-descriptions-item>
                  </a-descriptions>
                </a-space>
              </a-collapse-item>
            </a-collapse>
          </template>
        </a-form-item>
        <a-form-item field="tokenUri" label="Token 地址">
          <a-input
            v-model="createProviderForm.tokenUri"
            placeholder="请输入 Token 地址"
          />
        </a-form-item>
        <a-form-item field="enableCustomTokenReq" label="自定义获取 Token 请求">
          <a-switch
            v-model="createProviderForm.enableCustomTokenReq"
            type="round"
          />
        </a-form-item>
        <a-form-item
          v-if="createProviderForm.enableCustomTokenReq"
          field="tokenReqCfg"
          label="获取 Token 请求配置"
        >
          <monaco-editor
            v-model="createProviderForm.tokenReqCfg"
            language="json"
            :editorOption="{
              contextmenu: false,
            }"
            height="220px"
          />
          <template #extra>
            <a-collapse style="width: 100%">
              <a-collapse-item header="配置说明" key="1">
                <a-space direction="vertical" style="width: 100%">
                  <a-descriptions title="JSON 配置项" bordered :column="1">
                    <a-descriptions-item label="method">
                      必填项，类型为字符串（String），对应 HTTP
                      请求方式，可选值为：GET、POST。
                    </a-descriptions-item>
                    <a-descriptions-item label="params">
                      GET 请求时为必填项，类型为对象（Object），对应 query
                      参数，值支持从上下文中获取，获取方式为 ${XXX}，其中 XXX
                      为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="body">
                      POST
                      请求时为可选项，类型为对象（Object），对应请求体，值支持从上下文中获取，获取方式为
                      ${XXX}，其中 XXX 为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="headers">
                      可选项，类型为对象（Object），对应请求头，值支持从上下文中获取，获取方式为
                      ${XXX}，其中 XXX 为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="pathVariables">
                      可选项，类型为对象（Object），对应路径变量，值支持从上下文中获取，获取方式为
                      ${XXX}，其中 XXX 为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="accessTokenAttr">
                      可选项，类型为字符串（String），对应响应体中 AccessToken
                      的属性名，默认值为 access_token。
                    </a-descriptions-item>
                  </a-descriptions>
                  <a-descriptions title="可用的上下文变量" bordered :column="1">
                    <a-descriptions-item label="client_id">
                      身份源中的 Client ID。
                    </a-descriptions-item>
                    <a-descriptions-item label="client_secret">
                      身份源中的 Client Secret。
                    </a-descriptions-item>
                    <a-descriptions-item label="grant_type">
                      授权类型，固定值为 authorization_code。
                    </a-descriptions-item>
                    <a-descriptions-item label="code">
                      授权码。
                    </a-descriptions-item>
                    <a-descriptions-item label="redirect_uri">
                      授权回调地址。
                    </a-descriptions-item>
                    <a-descriptions-item label="其他">
                      身份源中的额外参数。
                    </a-descriptions-item>
                  </a-descriptions>
                </a-space>
              </a-collapse-item>
            </a-collapse>
          </template>
        </a-form-item>
        <a-form-item
          v-for="(i, index) in createProviderForm.userInfoUris"
          :key="index"
          :field="`userInfoUris[${index}]`"
          label="用户信息地址"
          tooltip="支持获取用户信息的多个地址"
          :hide-label="index !== 0"
          :rules="[{ required: true, message: '用户信息地址未填写' }]"
        >
          <a-input
            v-model="createProviderForm.userInfoUris[index]"
            placeholder="请输入用户信息地址"
          />
          <icon-minus-circle
            class="remove-userinfo-uri"
            @click="handleRemoveUserInfoUri(index)"
            v-if="index !== 0"
          />
        </a-form-item>
        <a-button
          type="text"
          size="mini"
          class="add-userinfo-uri-btn"
          @click="handleAddUserInfoUri"
        >
          <template #icon>
            <icon-plus-circle />
          </template>
          添加用户信息地址</a-button
        >
        <a-form-item
          field="enableCustomUserInfoReq"
          label="自定义获取用户信息请求"
        >
          <a-switch
            v-model="createProviderForm.enableCustomUserInfoReq"
            type="round"
          />
        </a-form-item>
        <a-form-item
          v-if="createProviderForm.enableCustomUserInfoReq"
          field="userInfoReqCfg"
          label="获取用户信息请求配置"
        >
          <monaco-editor
            v-model="createProviderForm.userInfoReqCfg"
            language="json"
            :editorOption="{
              contextmenu: false,
            }"
            height="220px"
          />
          <template #extra>
            <a-collapse style="width: 100%">
              <a-collapse-item header="配置说明" key="1">
                <a-space direction="vertical" style="width: 100%">
                  <a-descriptions title="JSON 配置项" bordered :column="1">
                    <a-descriptions-item label="说明">
                      支持对多个用户信息地址的请求进行配置，配置时以用户信息地址为
                      key，值为请求配置。
                    </a-descriptions-item>
                    <a-descriptions-item label="method">
                      必填项，类型为字符串（String），对应 HTTP
                      请求方式，可选值为：GET、POST。
                    </a-descriptions-item>
                    <a-descriptions-item label="params">
                      GET 请求时为必填项，类型为对象（Object），对应 query
                      参数，值支持从上下文中获取，获取方式为 ${XXX}，其中 XXX
                      为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="body">
                      POST
                      请求时为可选项，类型为对象（Object），对应请求体，值支持从上下文中获取，获取方式为
                      ${XXX}，其中 XXX 为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="headers">
                      可选项，类型为对象（Object），对应请求头，值支持从上下文中获取，获取方式为
                      ${XXX}，其中 XXX 为上下文变量名。
                    </a-descriptions-item>
                    <a-descriptions-item label="pathVariables">
                      可选项，类型为对象（Object），对应路径变量，值支持从上下文中获取，获取方式为
                      ${XXX}，其中 XXX 为上下文变量名。
                    </a-descriptions-item>
                  </a-descriptions>
                  <a-descriptions title="可用的上下文变量" bordered :column="1">
                    <a-descriptions-item label="client_id">
                      身份源中的 Client ID。
                    </a-descriptions-item>
                    <a-descriptions-item label="client_secret">
                      身份源中的 Client Secret。
                    </a-descriptions-item>
                    <a-descriptions-item label="access_token">
                      认证成功后获取到的 AccessToken。
                    </a-descriptions-item>
                    <a-descriptions-item label="其他">
                      身份源中的额外参数。
                    </a-descriptions-item>
                  </a-descriptions>
                </a-space>
              </a-collapse-item>
            </a-collapse>
          </template>
        </a-form-item>
        <a-form-item field="jwkSetUri" label="JWK 地址">
          <a-input
            v-model="createProviderForm.jwkSetUri"
            placeholder="请输入 JWK 地址"
          />
        </a-form-item>
        <a-row :gutter="24">
          <a-col :span="6">
            <a-form-item
              field="userInfoAuthenticationMethod"
              label="用户信息认证方式"
            >
              <a-radio-group
                v-model="createProviderForm.userInfoAuthenticationMethod"
              >
                <a-radio value="header">header</a-radio>
                <a-radio value="form">form</a-radio>
                <a-radio value="query">query</a-radio>
              </a-radio-group>
            </a-form-item>
          </a-col>
          <a-col :span="6">
            <a-form-item
              field="usernameAttribute"
              label="用户名属性"
            >
              <a-input
                v-model="createProviderForm.usernameAttribute"
                placeholder="请输入用户名属性"
              />
            </a-form-item>
          </a-col>
                          <a-col :span="6">
                  <a-form-item field="uniqueIdAttribute" label="唯一标识属性">
                    <a-input
                      v-model="createProviderForm.uniqueIdAttribute"
                      placeholder="请输入唯一标识属性"
                    />
                  </a-form-item>
                </a-col>
          <a-col :span="6">
            <a-form-item
              field="userMatchAttribute"
              label="用户匹配属性"
              tooltip="用于匹配用户列表中的用户"
            >
              <a-input
                v-model="createProviderForm.userMatchAttribute"
                placeholder="请输入用户匹配属性"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item
          v-for="(i, index) in createProviderForm.scopes"
          :key="index"
          :field="`scopes[${index}]`"
          label="scopes"
          :hide-label="index !== 0"
        >
          <a-input
            v-model="createProviderForm.scopes[index]"
            placeholder="请输入 scope"
          />
          <icon-minus-circle
            class="remove-scope"
            @click="handleRemoveScope(index)"
            v-if="index !== 0"
          />
        </a-form-item>
        <a-button
          type="text"
          size="mini"
          class="add-scope-btn"
          @click="handleAddScope"
        >
          <template #icon>
            <icon-plus-circle />
          </template>
          添加 scope</a-button
        >
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetCreateProviderForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
