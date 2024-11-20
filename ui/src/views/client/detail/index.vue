<script lang="ts">
import detailTs from "./index";
export default detailTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-header">
        {{ clientName }}
      </div>
      <a-tabs @change="handleTabChange" :active-key="activeTab">
        <a-tab-pane key="client_setting" title="客户端配置">
          <div class="tab-container">
            <div>
              <div class="info-title">基本信息</div>
              <a-form
                :model="clientBasicInfoForm"
                :rules="clientBasicInfoFormRules"
                layout="vertical"
                ref="clientBasicInfoFormRef"
                @submit-success="handleClientBasicInfoFormSubmit"
              >
                <a-form-item field="name" label="客户端名称">
                  <a-input
                    v-model="clientBasicInfoForm.name"
                    placeholder="请输入客户端名称"
                  />
                </a-form-item>
                <a-form-item field="desc" label="客户端描述">
                  <a-textarea
                    v-model="clientBasicInfoForm.desc"
                    placeholder="请输入客户端描述"
                    :auto-size="{
                      minRows: 3,
                      maxRows: 5,
                    }"
                  />
                </a-form-item>
                <a-form-item hide-label>
                  <a-space>
                    <a-button type="primary" html-type="submit">保存</a-button>
                    <a-button @click="handleResetClientBasicInfoForm"
                      >重置</a-button
                    >
                  </a-space>
                </a-form-item>
              </a-form>
            </div>
            <div class="info">
              <div class="info-title">端点信息</div>
              <a-descriptions :column="2" :align="{ label: 'right' }">
                <a-descriptions-item label="Client ID">
                  <copy-text :text="clientEndpointInfo.id"> </copy-text>
                </a-descriptions-item>
                <a-descriptions-item label="Client Secret">
                  <a-popconfirm
                    content="刷新后，原密钥将失效，确定刷新吗？"
                    @ok="handleUpdateClientSecretSubmit(clientEndpointInfo.id)"
                  >
                    <a-button type="text">
                      <template #icon>
                        <icon-refresh />
                      </template>
                      刷新密钥
                    </a-button>
                  </a-popconfirm>
                </a-descriptions-item>
                <a-descriptions-item label="Issuer">{{
                  clientEndpointInfo.issuer
                }}</a-descriptions-item>
                <a-descriptions-item label="OpenID Connect URL">
                  <a-link>
                    <a
                      :href="clientEndpointInfo.openidConfiguration"
                      target="_blank"
                      >{{ clientEndpointInfo.openidConfiguration }}</a
                    >
                    <icon-launch style="margin-left: 4px" />
                  </a-link>
                </a-descriptions-item>
                <a-descriptions-item label="JWKS 公钥端点">
                  <a-link>
                    <a :href="clientEndpointInfo.jwks" target="_blank">{{
                      clientEndpointInfo.jwks
                    }}</a>
                    <icon-launch style="margin-left: 4px" />
                  </a-link>
                </a-descriptions-item>
                <a-descriptions-item label="认证端点">
                  {{ clientEndpointInfo.authorize }}
                </a-descriptions-item>
                <a-descriptions-item label="Token 端点">
                  {{ clientEndpointInfo.token }}
                </a-descriptions-item>
                <a-descriptions-item label="用户信息端点">
                  {{ clientEndpointInfo.userinfo }}
                </a-descriptions-item>
              </a-descriptions>
            </div>
            <div class="info">
              <div class="info-title">认证配置</div>
              <a-form
                :model="clientAuthInfoForm"
                :rules="clientAuthInfoFormRules"
                ref="clientAuthInfoFormRef"
                @submit-success="handleClientAuthInfoFormSubmit"
                layout="vertical"
              >
                <a-form-item field="redirectUri" label="登录回调 URL">
                  <a-input
                    v-model="clientAuthInfoForm.redirectUri"
                    placeholder="请输入登录回调 URL"
                  />
                </a-form-item>
                <a-form-item hide-label>
                  <a-space>
                    <a-button type="primary" html-type="submit">保存</a-button>
                    <a-button @click="handleResetClientAuthInfoForm"
                      >重置</a-button
                    >
                  </a-space>
                </a-form-item>
              </a-form>
            </div>
            <div class="delete-container">
              <a-alert type="warning">
                <template #title> 删除 {{ clientName }} </template>
                此操作不可恢复，请谨慎操作！
                <template #action>
                  <a-button
                    type="primary"
                    status="danger"
                    @click="
                      handleDeleteClientSubmit(
                        clientName,
                        clientEndpointInfo.id
                      )
                    "
                    >删除此客户端</a-button
                  >
                </template>
              </a-alert>
            </div>
          </div>
        </a-tab-pane>
        <a-tab-pane key="oidc_setting" title="协议配置">
          <div class="tab-container">
            <div>
              <div class="info-title">授权配置</div>
              <a-form
                layout="vertical"
                :model="clientAuthorizeInfoForm"
                :rules="clientAuthorizeInfoFormRules"
                ref="clientAuthorizeInfoFormRef"
                @submit-success="handleClientAuthorizeInfoFormSubmit"
              >
                <a-form-item field="grantTypes" label="授权模式">
                  <a-checkbox-group
                    v-model="clientAuthorizeInfoForm.grantTypes"
                  >
                    <a-checkbox value="authorization_code"
                      >authorization_code</a-checkbox
                    >
                    <a-checkbox value="refresh_token">refresh_token</a-checkbox>
                    <a-checkbox value="client_credentials"
                      >client_credentials</a-checkbox
                    >
                    <a-checkbox value="password">password</a-checkbox>
                  </a-checkbox-group>
                </a-form-item>
                <a-form-item
                  field="authenticationMethods"
                  label="客户端认证方式"
                >
                  <a-checkbox-group
                    v-model="clientAuthorizeInfoForm.authenticationMethods"
                  >
                    <a-checkbox value="client_secret_basic"
                      >client_secret_basic</a-checkbox
                    >
                    <a-checkbox value="client_secret_post"
                      >client_secret_post</a-checkbox
                    >
                    <a-checkbox value="none">none</a-checkbox>
                  </a-checkbox-group>
                </a-form-item>
                <a-row :gutter="24">
                  <a-col :span="12">
                    <a-form-item
                      field="authorizationCodeTimeToLive"
                      label="授权码过期时间"
                    >
                      <a-input-group>
                        <a-input-number
                          v-model="
                            clientAuthorizeInfoForm.authorizationCodeTimeToLive
                          "
                          :min="1"
                        />
                        <a-select
                          v-model="authorizationCodeTimeToLiveUnit"
                          :style="{ width: '120px' }"
                        >
                          <a-option :value="1">分钟</a-option>
                          <a-option :value="60">小时</a-option>
                          <a-option :value="1440">天</a-option>
                        </a-select>
                      </a-input-group>
                    </a-form-item>
                  </a-col>
                  <a-col :span="12">
                    <a-form-item
                      field="accessTokenTimeToLive"
                      label="访问令牌过期时间"
                    >
                      <a-input-group>
                        <a-input-number
                          v-model="
                            clientAuthorizeInfoForm.accessTokenTimeToLive
                          "
                          :min="1"
                        />
                        <a-select
                          v-model="accessTokenTimeToLiveUnit"
                          :style="{ width: '120px' }"
                        >
                          <a-option :value="1">分钟</a-option>
                          <a-option :value="60">小时</a-option>
                          <a-option :value="1440">天</a-option>
                        </a-select>
                      </a-input-group>
                    </a-form-item>
                  </a-col>
                </a-row>
                <a-form-item
                  field="refreshTokenTimeToLive"
                  label="刷新令牌过期时间"
                >
                  <a-input-group>
                    <a-input-number
                      v-model="clientAuthorizeInfoForm.refreshTokenTimeToLive"
                      :min="1"
                    />
                    <a-select
                      v-model="refreshTokenTimeToLiveUnit"
                      :style="{ width: '120px' }"
                    >
                      <a-option :value="1">分钟</a-option>
                      <a-option :value="60">小时</a-option>
                      <a-option :value="1440">天</a-option>
                    </a-select>
                  </a-input-group>
                </a-form-item>
                <a-form-item label="OIDC scope">
                  <div class="oidc-scope">
                    <a-checkbox-group
                      direction="vertical"
                      v-model="clientAuthorizeInfoForm.scopes"
                    >
                      <div
                        class="scope-container"
                        v-for="(scope, index) in oidcScopes"
                        :key="scope.name"
                      >
                        <a-checkbox :value="scope.name" />
                        <div class="scope-content">
                          <a-row :gutter="24" align="center">
                            <a-col :span="8">
                              <input-text
                                v-model="scope.name"
                                placeholder="请输入 Scope 名称"
                              />
                            </a-col>
                            <a-col :span="12">
                              <div class="scope-claim">
                                <div
                                  style="margin-right: 12px; user-select: none"
                                >
                                  claim:
                                </div>
                                <a-select
                                  placeholder="请选择 OIDC claim"
                                  multiple
                                  v-model="scope.claims"
                                >
                                  <a-option
                                    v-for="(claim, index) in oidcClaims"
                                    :key="claim.name"
                                    :value="claim.id"
                                  >
                                    {{ claim.name }}
                                  </a-option>
                                </a-select>
                              </div>
                            </a-col>
                            <a-col :span="4">
                              <a-space>
                                <a-button
                                  type="text"
                                  size="mini"
                                  @click="handleSaveOidcScopeSubmit(scope)"
                                >
                                  保存
                                  <template #icon>
                                    <icon-save />
                                  </template>
                                </a-button>
                                <a-popconfirm
                                  v-if="scope.id"
                                  content="删除后，所有客户端将失去该 scope 的访问权限，确定删除吗？"
                                  :ok-button-props="{ status: 'danger' }"
                                  @ok="handleDeleteOidcScopeSubmit(scope)"
                                >
                                  <a-button
                                    type="text"
                                    size="mini"
                                    status="danger"
                                  >
                                    删除
                                    <template #icon>
                                      <icon-delete />
                                    </template>
                                  </a-button>
                                </a-popconfirm>
                                <a-button
                                  type="text"
                                  size="mini"
                                  status="danger"
                                  v-else
                                  @click="handleRemoveOidcScope(scope)"
                                >
                                  移除
                                  <template #icon>
                                    <icon-minus-circle />
                                  </template>
                                </a-button>
                              </a-space>
                            </a-col>
                          </a-row>
                        </div>
                      </div>
                    </a-checkbox-group>
                    <div class="btn-container">
                      <a-button type="text" @click="handleCreateOidcScope">
                        <template #icon>
                          <icon-plus />
                        </template>
                        新建自定义 Scope
                      </a-button>
                    </div>
                  </div>
                </a-form-item>
                <a-form-item hide-label>
                  <a-space>
                    <a-button type="primary" html-type="submit">保存</a-button>
                    <a-button @click="handleResetClientAuthorizeInfoForm"
                      >重置</a-button
                    >
                  </a-space>
                </a-form-item>
              </a-form>
              <div class="info-title">Claim 字段映射配置</div>
              <div
                class="claim-container"
                v-for="(claim, index) in claimMappings"
                :key="claim.id"
              >
                <a-row :gutter="24" align="center" style="width: 100%">
                  <a-col :span="8">
                    <input-text
                      v-model="claim.name"
                      placeholder="请输入 Claim 名称"
                    />
                  </a-col>
                  <a-col :span="12">
                    <div class="claim-attr">
                      <div
                        style="
                          margin-right: 14px;
                          user-select: none;
                          width: 35px;
                        "
                      >
                        字段:
                      </div>
                      <a-select
                        placeholder="请选择用户字段"
                        v-model="claim.userAttrId"
                      >
                        <a-option
                          v-for="(userAttr, index) in userAttrs"
                          :key="userAttr.id"
                          :value="userAttr.id"
                        >
                          {{ userAttr.name }} - {{ userAttr.key }}
                        </a-option>
                      </a-select>
                    </div>
                  </a-col>
                  <a-col :span="4">
                    <a-space>
                      <a-button
                        type="text"
                        size="mini"
                        @click="handleSaveOidcClaimSubmit(claim)"
                      >
                        保存
                        <template #icon>
                          <icon-save />
                        </template>
                      </a-button>
                      <a-popconfirm
                        v-if="claim.id"
                        content="删除后，该 claim 将从 scope 中移除，确定删除吗？"
                        :ok-button-props="{ status: 'danger' }"
                        @ok="handleDeleteOidcClaimSubmit(claim)"
                      >
                        <a-button type="text" size="mini" status="danger">
                          删除
                          <template #icon>
                            <icon-delete />
                          </template>
                        </a-button>
                      </a-popconfirm>
                      <a-button
                        type="text"
                        size="mini"
                        status="danger"
                        @click="handleRemoveOidcClaim"
                        v-else
                      >
                        移除
                        <template #icon>
                          <icon-minus-circle />
                        </template>
                      </a-button>
                    </a-space>
                  </a-col>
                </a-row>
              </div>
              <div class="btn-container">
                <a-button type="text" @click="handleCreateOidcClaim">
                  <template #icon>
                    <icon-plus />
                  </template>
                  新建自定义 Claim
                </a-button>
              </div>
            </div>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>

    <a-modal
      v-model:visible="updateClientSecretSuccessModalVisible"
      hide-cancel
      :footer="false"
      :mask-closable="false"
      :width="660"
    >
      <template #title>
        <div>
          <icon-check-circle-fill
            style="color: rgb(var(--green-6)); margin-right: 4px"
          />
          刷新密钥成功
        </div>
      </template>
      <div class="secret-modal">
        <copy-text :text="newClientSecret" />
        <div class="info-text">
          新的密钥仅显示一次，所有使用原密钥的客户端均需要更新，请及时保存。
        </div>
      </div>
    </a-modal>
  </div>
</template>
