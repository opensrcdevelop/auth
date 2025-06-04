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
      <div class="detail-header">
        <div>
          <span class="title">{{ providerName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="providerId" textColor="#86909c" />
          </div>
        </div>
        <a-space>
          <a-dropdown :popup-max-height="false">
            <a-button>更多</a-button>
            <template #content>
              <a-doption style="color: #e8353e" @click="handleDeleteProvider">
                <icon-delete />
                <span style="margin-left: 8px">删除身份源提供商</span>
              </a-doption>
            </template>
          </a-dropdown>
          <a-button type="primary" @click="handleToCreateRegistration"
            >创建身份源</a-button
          >
        </a-space>
      </div>

      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="provider_info" title="身份源提供商信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="providerBasicInfoForm"
              ref="providerBasicInfoFormRef"
              :rules="providerBasicInfoFormRules"
              layout="vertical"
              @submit-success="handleProviderBasicInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="身份源提供商名称">
                    <a-input
                      v-model="providerBasicInfoForm.name"
                      placeholder="请输入身份源提供商名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="code" label="身份源提供商标识">
                    <a-input
                      v-model="providerBasicInfoForm.code"
                      placeholder="请输入身份源提供商标识"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="logo" label="身份源提供商 Logo 地址">
                <a-input
                  v-model="providerBasicInfoForm.logo"
                  placeholder="身份源提供商 Logo 地址"
                />
              </a-form-item>
              <a-form-item field="desc" label="身份源提供商描述">
                <a-textarea
                  v-model="providerBasicInfoForm.desc"
                  placeholder="请输入身份源提供商描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetProviderBasicInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
            <div class="info-title">端点信息</div>
            <a-form
              :model="providerEndpointInfoForm"
              ref="providerEndpointInfoFormRef"
              :rules="providerEndpointInfoFormRules"
              layout="vertical"
              @submit-success="handleProviderEndpointInfoFormSubmit"
            >
              <a-form-item field="authorizationUri" label="授权地址">
                <a-input
                  v-model="providerEndpointInfoForm.authorizationUri"
                  placeholder="请输入授权地址"
                />
              </a-form-item>
              <a-form-item field="enableCustomAuthzReq" label="自定义授权请求">
                <a-switch
                  v-model="providerEndpointInfoForm.enableCustomAuthzReq"
                  type="round"
                />
              </a-form-item>
              <a-form-item
                v-if="providerEndpointInfoForm.enableCustomAuthzReq"
                field="authzReqCfg"
                label="授权请求配置"
              >
                <monaco-editor
                  v-model="providerEndpointInfoForm.authzReqCfg"
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
                        <a-descriptions
                          title="JSON 配置项"
                          bordered
                          :column="1"
                        >
                          <a-descriptions-item label="params">
                            必填项，类型为对象（Object），对应授权请求地址的
                            query 参数，值支持从上下文中获取，获取方式为
                            ${XXX}，其中 XXX 为上下文变量名。
                          </a-descriptions-item>
                        </a-descriptions>
                        <a-descriptions
                          title="可用的上下文变量"
                          bordered
                          :column="1"
                        >
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
                  v-model="providerEndpointInfoForm.tokenUri"
                  placeholder="请输入 Token 地址"
                />
              </a-form-item>
              <a-form-item
                field="enableCustomTokenReq"
                label="自定义获取 Token 请求"
              >
                <a-switch
                  v-model="providerEndpointInfoForm.enableCustomTokenReq"
                  type="round"
                />
              </a-form-item>
              <a-form-item
                v-if="providerEndpointInfoForm.enableCustomTokenReq"
                field="tokenReqCfg"
                label="获取 Token 请求配置"
              >
                <monaco-editor
                  v-model="providerEndpointInfoForm.tokenReqCfg"
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
                        <a-descriptions
                          title="JSON 配置项"
                          bordered
                          :column="1"
                        >
                          <a-descriptions-item label="method">
                            必填项，类型为字符串（String），对应 HTTP
                            请求方式，可选值为：GET、POST。
                          </a-descriptions-item>
                          <a-descriptions-item label="params">
                            GET 请求时为必填项，类型为对象（Object），对应 query
                            参数，值支持从上下文中获取，获取方式为 ${XXX}，其中
                            XXX 为上下文变量名。
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
                            可选项，类型为字符串（String），对应响应体中
                            AccessToken 的属性名，默认值为 access_token。
                          </a-descriptions-item>
                        </a-descriptions>
                        <a-descriptions
                          title="可用的上下文变量"
                          bordered
                          :column="1"
                        >
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
                v-for="(i, index) in providerEndpointInfoForm.userInfoUris"
                :key="index"
                :field="`userInfoUris[${index}]`"
                label="用户信息地址"
                tooltip="支持获取用户信息的多个地址"
                :hide-label="index !== 0"
                :rules="[{ required: true, message: '用户信息地址未填写' }]"
              >
                <a-input
                  v-model="providerEndpointInfoForm.userInfoUris[index]"
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
                  v-model="providerEndpointInfoForm.enableCustomUserInfoReq"
                  type="round"
                />
              </a-form-item>
              <a-form-item
                v-if="providerEndpointInfoForm.enableCustomUserInfoReq"
                field="userInfoReqCfg"
                label="获取用户信息请求配置"
              >
                <monaco-editor
                  v-model="providerEndpointInfoForm.userInfoReqCfg"
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
                        <a-descriptions
                          title="JSON 配置项"
                          bordered
                          :column="1"
                        >
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
                            参数，值支持从上下文中获取，获取方式为 ${XXX}，其中
                            XXX 为上下文变量名。
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
                        <a-descriptions
                          title="可用的上下文变量"
                          bordered
                          :column="1"
                        >
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
                  v-model="providerEndpointInfoForm.jwkSetUri"
                  placeholder="请输入 JWK 地址"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetProviderEndpointInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
            <div class="info-title">认证信息</div>
            <a-form
              :model="providerAuthInfoForm"
              ref="providerAuthInfoFormRef"
              layout="vertical"
              :rules="providerAuthInfoFormRules"
              @submit-success="handleProviderAuthInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="8">
                  <a-form-item
                    field="userInfoAuthenticationMethod"
                    label="用户信息认证方式"
                  >
                    <a-radio-group
                      v-model="
                        providerAuthInfoForm.userInfoAuthenticationMethod
                      "
                    >
                      <a-radio value="header">header</a-radio>
                      <a-radio value="form">form</a-radio>
                      <a-radio value="query">query</a-radio>
                    </a-radio-group>
                  </a-form-item>
                </a-col>
                <a-col :span="8">
                  <a-form-item
                    field="usernameAttribute"
                    label="用户名属性"
                    tooltip="用于唯一标识第三方用户"
                  >
                    <a-input
                      v-model="providerAuthInfoForm.usernameAttribute"
                      placeholder="请输入用户名属性"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="8">
                  <a-form-item
                    field="userMatchAttribute"
                    label="用户匹配属性"
                    tooltip="用于匹配用户列表中的用户"
                  >
                    <a-input
                      v-model="providerAuthInfoForm.userMatchAttribute"
                      placeholder="请输入用户匹配属性"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item
                v-for="(i, index) in providerAuthInfoForm.scopes"
                :key="index"
                :field="`scopes[${index}]`"
                label="scopes"
                :hide-label="index !== 0"
              >
                <a-input
                  v-model="providerAuthInfoForm.scopes[index]"
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
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetProviderAuthInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="registration_list" title="关联身份源">
          <div class="tab-container">
            <div class="info-title">关联身份源</div>
            <a-table
              :data="registrationList"
              :bordered="false"
              :scroll="{ y: '100%' }"
              :pagination="false"
            >
              <template #columns>
                <a-table-column
                  title="身份源显示名称"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToRegistrationDetail(record)"
                    >
                      {{ record.name }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column
                  title="身份源标识"
                  ellipsis
                  tooltip
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span>
                      {{ record.code }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="状态">
                  <template #cell="{ record }">
                    <a-switch
                      v-model="record.enabled"
                      type="round"
                      size="small"
                      @change="handleUpdateRegistrationState(record)"
                    />
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>
  </div>
</template>
