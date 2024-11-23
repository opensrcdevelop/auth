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
        <div>
          <span class="title">{{ permissionExpName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="permissionExpId" textColor="#86909c" />
          </div>
        </div>
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
              <a-form-item field="expression" label="SpringEL 表达式">
                <a-textarea
                  v-model="permissionExpInfoForm.expression"
                  placeholder="请输入 SpringEL 表达式"
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
  </div>
</template>
