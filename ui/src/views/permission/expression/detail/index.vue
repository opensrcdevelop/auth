<script lang="ts">
import detailTs from "./index";

export default detailTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-header">
        <div>
          <span class="title">{{ permissionExpName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="permissionExpId" textColor="#86909c" />
          </div>
        </div>
        <a-button
          type="primary"
          v-if="!permissionExpInfoForm.templateId"
          @click="handleOpenDebugDrawer"
        >
          调试运行
        </a-button>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="condition_info" title="限制条件信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="permissionExpInfoForm"
              ref="permissionExpInfoFormRef"
              :rules="permissionExpInfoFormRules"
              layout="vertical"
              @submit-success="handlePermissionExpInfoFormSubmit"
            >
              <a-form-item field="name" label="限制条件名称">
                <a-input
                  v-model="permissionExpInfoForm.name"
                  placeholder="请输入限制条件名称"
                />
              </a-form-item>
              <a-form-item
                v-if="permissionExpInfoForm.templateId"
                field="templateId"
                label="限制条件模板"
              >
                <a-select
                  placeholder="请选择限制条件模板"
                  disabled
                  v-model="permissionExpInfoForm.templateId"
                >
                  <a-option v-for="item in templateList" :value="item.id">
                    {{ item.name }}
                  </a-option>
                </a-select>
              </a-form-item>
              <div v-if="templateParamConfigs.length > 0">
                <ParamInput
                  ref="templateParamsRef"
                  :configs="templateParamConfigs"
                  v-model="permissionExpInfoForm.templateParams"
                />
              </div>
              <a-form-item
                v-if="!permissionExpInfoForm.templateId"
                field="expression"
                label="JEXL 表达式"
              >
                <monaco-editor
                  v-model="permissionExpInfoForm.expression"
                  language="jexl"
                  :editorOption="{
                    contextmenu: false,
                  }"
                  height="280px"
                />
              </a-form-item>
              <a-form-item field="desc" label="限制条件描述">
                <a-textarea
                  v-model="permissionExpInfoForm.desc"
                  placeholder="请输入限制条件描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetPermissionExpInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="permission_list" title="关联权限">
          <div class="tab-container">
            <div class="info-title">关联权限</div>
            <a-table :data="permissions" :bordered="false" :pagination="false">
              <template #columns>
                <a-table-column
                  title="被授权主体"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handeToPrincipalDetail(record)"
                      >{{ record.principal }}</span
                    >
                  </template>
                </a-table-column>
                <a-table-column
                  title="主体类型"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    {{ record.principalTypeDisplayName }}
                  </template>
                </a-table-column>
                <a-table-column
                  title="资源组"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="
                        handleToResourceGroupDetail(record.resourceGroupId)
                      "
                      >{{ record.resourceGroupName }}</span
                    >
                  </template>
                </a-table-column>
                <a-table-column
                  title="资源"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToResourceDetail(record.resourceId)"
                      >{{ record.resourceName }}</span
                    >
                  </template>
                </a-table-column>
                <a-table-column
                  title="权限名称"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToPermissionDetail(record.permissionId)"
                      >{{ record.permissionName }}</span
                    >
                  </template>
                </a-table-column>
                <a-table-column
                  title="权限标识"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                >
                  <template #cell="{ record }">
                    {{ record.permissionCode }}
                  </template>
                </a-table-column>
                <a-table-column title="操作">
                  <template #cell="{ record }">
                    <a-dropdown>
                      <a-button type="text">
                        <template #icon>
                          <icon-more />
                        </template>
                      </a-button>
                      <template #content>
                        <a-doption
                          style="color: #e8353e"
                          @click="
                            handleRemoveAuthorizeCondition(record.authorizeId)
                          "
                        >
                          <template #icon>
                            <icon-undo />
                          </template>
                          取消限制</a-doption
                        >
                      </template>
                    </a-dropdown>
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>

    <a-drawer
      :width="540"
      :visible="debugDrawerVisible"
      ok-text="调试运行"
      :ok-loading="debugFormSubmitLoading"
      @cancel="handleCloseDebugDrawer"
      @ok="handleDebugFormSubmit"
    >
      <template #title>调试运行限制条件</template>
      <a-form
        :model="debugForm"
        :rules="debugFormRules"
        ref="debugFormRef"
        layout="vertical"
      >
        <a-form-item field="context" label="上下文">
          <monaco-editor
            v-model="debugForm.context"
            language="json"
            height="280px"
            :editorOption="{
              contextmenu: false,
            }"
          />
          <template #extra>
            <div>
              上下文为 JSON 对象格式，请确保输入的上下文符合 JSON 格式要求
            </div>
          </template>
        </a-form-item>
      </a-form>
    </a-drawer>

    <a-modal
      :visible="debugResultModalVisible"
      :footer="false"
      :mask-closable="false"
      @cancel="debugResultModalVisible = false"
    >
      <template #title>调试运行结果</template>
      <a-descriptions :column="1" bordered>
        <a-descriptions-item label="状态">
          {{ debugResult.success ? "成功" : "失败" }}
        </a-descriptions-item>
        <a-descriptions-item label="结果" v-if="debugResult.success">
          <a-tag v-if="debugResult.execResult" color="#00b42a">
            <template #icon>
              <icon-check-circle-fill style="color: #fff" />
            </template>
            允许
          </a-tag>
          <a-tag v-else color="#f53f3f">
            <template #icon>
              <icon-minus-circle-fill style="color: #fff" />
            </template>
            拒绝
          </a-tag>
        </a-descriptions-item>
        <a-descriptions-item label="错误" v-else>
          {{ debugResult.execResult }}
        </a-descriptions-item>
      </a-descriptions>
    </a-modal>
  </div>
</template>
