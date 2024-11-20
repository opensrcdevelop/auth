<script lang="ts">
import authorizeTs from "./index";
export default authorizeTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建授权</div>
      <div class="info-title">资源组</div>
      <div class="resource-group-container">
        <a-row :gutter="24">
          <a-col :span="12">
            <a-input :model-value="resourceGroup.name" disabled />
          </a-col>
        </a-row>
      </div>
      <div class="info-title" v-if="princialSelectable">授权主体</div>
      <a-form
        v-if="princialSelectable"
        :model="principalForm"
        :rules="principalFormRules"
        layout="vertical"
        ref="principalFormRef"
        ><a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="type" label="主体类型">
              <a-radio-group v-model="principalForm.type" @change="principalSelectChange">
                <a-radio value="USER">用户</a-radio>
                <a-radio value="USER_GROUP">用户组</a-radio>
                <a-radio value="ROLE">角色</a-radio>
              </a-radio-group>
            </a-form-item>
          </a-col>
        </a-row>
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="id" label="主体">
              <a-select
                v-if="principalForm.type === 'USER'"
                placeholder="请选择用户"
                allow-clear
                allow-search
                multiple
                v-model:model-value="principalForm.id"
                v-model:input-value="userSearchKeyword"
                @search="handleSearchUser"
                @clear="handleSearchUser"
                :filter-option="false"
                @dropdown-reach-bottom="loadMoreUser"
              >
                <a-option
                  v-for="user in userList"
                  :key="user.userId"
                  :value="user.userId"
                >
                  {{ user.username }}
                </a-option>
              </a-select>
              <a-select
                v-if="principalForm.type === 'USER_GROUP'"
                placeholder="请选择用户组"
                allow-clear
                allow-search
                multiple
                v-model:model-value="principalForm.id"
                v-model:input-value="userGroupSearchKeyword"
                @search="handleSearchUserGroup"
                @clear="handleSearchUserGroup"
                :filter-option="false"
                @dropdown-reach-bottom="loadMoreUserGroup"
              >
                <a-option
                  v-for="userGroup in userGroupList"
                  :key="userGroup.id"
                  :value="userGroup.id"
                >
                  {{ userGroup.name }}
                </a-option>
              </a-select>
              <a-select
                v-if="principalForm.type === 'ROLE'"
                placeholder="请选择角色"
                allow-clear
                allow-search
                multiple
                v-model:model-value="principalForm.id"
                v-model:input-value="roleSearchKeyword"
                @search="handleSearchRole"
                @clear="handleSearchRole"
                :filter-option="false"
                @dropdown-reach-bottom="loadMoreRole"
              >
                <a-option
                  v-for="role in roleList"
                  :key="role.id"
                  :value="role.id"
                >
                  {{ role.name }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
        </a-row>
      </a-form>
      <div class="info-title">权限授权</div>
      <a-form
        :model="authorizeForm"
        :rules="authorizeFormRules"
        layout="vertical"
        ref="authorizeFormRef"
        @submit="handleAuthorizeFormSubmit"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="resourceId" label="资源">
              <a-select
                placeholder="请选择资源"
                allow-clear
                allow-search
                v-model:model-value="authorizeForm.resourceId"
                v-model:input-value="resourceSearchKeyword"
                @change="handleSearchPermission"
                @search="handleSearchResource"
                @clear="handleSearchResource"
                :filter-option="false"
                @dropdown-reach-bottom="loadMoreResource"
              >
                <a-option
                  v-for="resource in resourceList"
                  :key="resource.id"
                  :value="resource.id"
                >
                  {{ resource.name }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="permissionIds" label="权限">
              <a-select
                placeholder="请选择权限"
                multiple
                allow-clear
                allow-search
                v-model:model-value="authorizeForm.permissionIds"
                v-model:input-value="permissionSearchKeyword"
                @popup-visible-change="handlePermissionSelectVisibleChange"
                @search="handleSearchPermission"
                @clear="handleSearchPermission"
                :filter-option="false"
                @dropdown-reach-bottom="loadMorePermission"
                :disabled="!authorizeForm.resourceId"
              >
                <a-option
                  v-for="permission in permissionList"
                  :key="permission.permissionId"
                  :value="permission.permissionId"
                >
                  {{ permission.permissionName }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="expressionIds" label="限制条件">
              <a-select
                placeholder="请选择限制条件"
                multiple
                allow-clear
                allow-search
                v-model:model-value="authorizeForm.expressionIds"
                v-model:input-value="authorizeConditionSearchKeyword"
                @search="handleSearchAuthorizeCondition"
                @clear="handleSearchAuthorizeCondition"
                :filter-option="false"
                @dropdown-reach-bottom="loadMoreAuthorizeCondition"
                :disabled="authorizeForm.permissionIds.length === 0"
              >
                <a-option
                  v-for="permission in authorizeConditionList"
                  :key="permission.id"
                  :value="permission.id"
                >
                  {{ permission.name }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item hide-label>
          <a-space>
            <a-button type="primary" html-type="submit">创建</a-button>
            <a-button @click="handleResetAuthorizeForm">重置</a-button>
          </a-space>
        </a-form-item>
      </a-form>
    </page-header>
  </div>
</template>
