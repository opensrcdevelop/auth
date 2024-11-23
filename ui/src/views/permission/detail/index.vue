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
          <span class="title">{{ permissionName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="permissionId" textColor="#86909c" />
          </div>
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="permission_info" title="权限信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="permissionInfoForm"
              ref="permissionInfoFormRef"
              :rules="permissionInfoFormRules"
              layout="vertical"
              @submit-success="permissionInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="权限名称">
                    <a-input
                      v-model="permissionInfoForm.name"
                      placeholder="请输入资源名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="code" label="权限标识">
                    <a-input
                      v-model="permissionInfoForm.code"
                      placeholder="请输入资源标识"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="desc" label="权限描述">
                <a-textarea
                  v-model="permissionInfoForm.desc"
                  placeholder="请输入资源描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetPermissionInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="authorization_management" title="授权管理">
          <div class="tab-container">
            <a-input-search
              :style="{ width: '320px' }"
              placeholder="搜索主体"
              allow-clear
              v-model="authorizedPrincipalSearchKeyword"
              @search="handleSerachAuthorizedPrincipal"
              @keyup.enter.native="handleSerachAuthorizedPrincipal"
              @clear="handleSerachAuthorizedPrincipal"
            />
            <div class="record-list">
              <a-table
                :data="authorizeRecords"
                :bordered="false"
                :pagination="false"
                :expandable="{ width: 30 }"
                row-key="authorizeId"
              >
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
                    title="授权时间"
                    :width="180"
                    :sortable="{
                      sortDirections: ['ascend', 'descend'],
                    }"
                  >
                    <template #cell="{ record }">
                      {{ record.authorizeTime }}
                    </template>
                  </a-table-column>
                  <a-table-column title="操作" :width="60">
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
                              handleCancelAuthorization(record.principalId)
                            "
                          >
                            <template #icon>
                              <icon-undo />
                            </template>
                            取消授权</a-doption
                          >
                        </template>
                      </a-dropdown>
                    </template>
                  </a-table-column>
                </template>
                <template #expand-row="{ record }">
                  <div class="condition-container">
                    <div v-if="record.conditions.length > 0">限制条件</div>
                    <div v-else>无限制条件</div>
                    <div v-if="record.conditions.length > 0">
                      <div
                        class="expression-container"
                        v-for="(condition, index) in record.conditions"
                      >
                        <a-descriptions
                          style="width: 100%"
                          :key="index"
                          :column="1"
                          bordered
                        >
                          <a-descriptions-item label="限制条件名称">
                            {{ condition.name }}
                          </a-descriptions-item>
                          <a-descriptions-item label="SpringEL 表达式">
                            {{ condition.expression }}
                          </a-descriptions-item>
                        </a-descriptions>
                        <div
                          class="expression-remove"
                          @click="
                            handleRemoveAuthorizeCondition(record, condition)
                          "
                        >
                          <icon-minus-circle-fill />
                        </div>
                      </div>
                    </div>
                    <a-button
                      type="text"
                      style="margin-top: 16px"
                      @click="handleOpenAddAuthorizeConditionModal(record)"
                    >
                      <template #icon>
                        <icon-plus />
                      </template>
                      添加限制条件
                    </a-button>
                  </div>
                </template>
                <template #expand-icon="{ expanded }">
                  <icon-right v-if="!expanded" />
                  <icon-down v-if="expanded" />
                </template>
              </a-table>
            </div>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>

    <!-- 添加限制条件对话框 -->
    <a-modal
      :visible="addAuthorizeConditionModalVisible"
      @cancel="handleCloseAddAuthorizeConditionModal"
      :footer="false"
    >
      <template #title>限制条件</template>
      <a-form
        :model="addAuthorizeConditionForm"
        ref="addAuthorizeConditionFormRef"
        :rules="addAuthorizeConditionFormRules"
        @submit-success="handleAddAuthorizeConditionFormSubmit"
      >
        <a-form-item field="permissionExpIds" label="限制条件">
          <a-select
            v-model:model-value="addAuthorizeConditionForm.permissionExpIds"
            v-model:input-value="authorizeConditionSearchKeyword"
            multiple
            allow-clear
            allow-search
            placeholder="请选择限制条件"
            :filter-option="false"
            @search="handleSearchAuthorizeCondition"
            @dropdown-reach-bottom="loadMoreAuthorizeCondition"
          >
            <a-option
              v-for="(condition, index) in authorizeConditionList"
              :key="condition.id"
              :value="condition.id"
            >
              {{ condition.name }}
            </a-option>
          </a-select>
        </a-form-item>
        <a-form-item>
          <div class="add-btn-container">
            <a-space>
              <a-button @click="handleCloseAddAuthorizeConditionModal"
                >取消</a-button
              >
              <a-button
                type="primary"
                html-type="submit"
                :loading="addAuthorizeConditionFormSubmitLoading"
                >添加</a-button
              >
            </a-space>
          </div>
        </a-form-item>
      </a-form>
    </a-modal>
  </div>
</template>
