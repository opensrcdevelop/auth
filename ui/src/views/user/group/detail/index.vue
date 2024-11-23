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
        <span class="title">{{ userGroupName }}</span>
        <div class="id">
          <span>ID:</span>
          <copy-text :text="userGroupId" textColor="#86909c" />
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="user_group_info" title="用户组信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="userGroupInfoForm"
              layout="vertical"
              ref="userGroupInfoFormRef"
              :rules="userGroupInfoFormRules"
              @submit-success="handleUserGroupInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="用户组名称">
                    <a-input
                      v-model="userGroupInfoForm.name"
                      placeholder="请输入用户组名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="code" label="用户组标识">
                    <a-input
                      v-model="userGroupInfoForm.code"
                      placeholder="请输入用户组标识"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="desc" label="用户组描述">
                <a-textarea
                  v-model="userGroupInfoForm.desc"
                  placeholder="请输入用户组描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetUserGroupInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
            <div class="info-title">用户组成员</div>
            <a-input-search
              :style="{ width: '320px' }"
              placeholder="输入用户名 / 邮箱 / 手机号进行搜索"
              allow-clear
              v-model="searchGroupUserKeyword"
              @search="handleSearchGroupUser"
              @clear="handleSearchGroupUser"
              @keyup.enter.native="handleSearchGroupUser"
            />
            <div class="add-container">
              <a-button type="text" @click="handleOpenAddGroupUserModal">
                <template #icon>
                  <icon-plus />
                </template>
                添加成员
              </a-button>
            </div>
            <a-table
              :data="groupUsers"
              :pagination="groupUsersPagination"
              :bordered="false"
              :scroll="{ y: '100%' }"
              @page-size-change="handlePageSizeChange"
              @page-change="handlePageChange"
            >
              <template #columns>
                <a-table-column title="用户名">
                  <template #cell="{ record }">
                    <span
                      class="table-column-name"
                      @click="handleToUserDetail(record)"
                      >{{ record.username }}</span
                    >
                  </template>
                </a-table-column>
                <a-table-column title="邮箱">
                  <template #cell="{ record }">
                    {{ record.emailAddress ? record.emailAddress : "-" }}
                  </template>
                </a-table-column>
                <a-table-column title="手机">
                  <template #cell="{ record }">
                    {{ record.phoneNumber ? record.phoneNumber : "-" }}
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
                          @click="handleRemoveGroupUser(record)"
                        >
                          <template #icon>
                            <icon-undo />
                          </template>
                          移除</a-doption
                        >
                      </template>
                    </a-dropdown>
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
        <a-tab-pane key="permission_management" title="权限管理">
          <div class="tab-container">
            <div class="info-title">权限授权</div>
            <div class="add-container">
              <a-button type="text" @click="handleAuthorize">
                <template #icon>
                  <icon-plus />
                </template>
                授权
              </a-button>
            </div>
            <a-table
              :data="permissions"
              :bordered="false"
              :pagination="permissionsPagination"
              @page-change="handlePermissionsPageChange"
              @page-size-change="handlePermissionsPageSizeChange"
              :expandable="{ width: 30 }"
              row-key="permissionId"
            >
              <template #columns>
                <a-table-column
                  title="资源组"
                  :sortable="{
                    sortDirections: ['ascend', 'descend'],
                  }"
                  :filterable="resourceGroupNameFilter"
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
                  :filterable="resourceNameFilter"
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
                  :filterable="permissionNameFilter"
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
                  :filterable="permissionCodeFilter"
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
                          @click="handleCancelAuthorization(record)"
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
                    <a-descriptions
                      style="margin-top: 12px"
                      v-for="(condition, index) in record.conditions"
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
                  </div>
                </div>
              </template>
              <template #expand-icon="{ expanded }">
                <icon-right v-if="!expanded" />
                <icon-down v-if="expanded" />
              </template>
              <template #resource-group-name-filter>
                <div class="permission-filter">
                  <a-input
                    placeholder="输入资源组名称进行搜索"
                    v-model="authorizeSearchKeywords.resourceGroupName"
                  />
                  <div class="filter-footer">
                    <a-space>
                      <a-button
                        @click="handleGetUserGroupPermissions(1, 15)"
                        type="primary"
                        >确认</a-button
                      >
                      <a-button
                        @click="
                          handleResetPermissionFilter('resourceGroupName')
                        "
                        >重置</a-button
                      >
                    </a-space>
                  </div>
                </div>
              </template>
              <template #resource-name-filter>
                <div class="permission-filter">
                  <a-input
                    placeholder="输入资源名称进行搜索"
                    v-model="authorizeSearchKeywords.resourceName"
                  />
                  <div class="filter-footer">
                    <a-space>
                      <a-button
                        @click="handleGetUserGroupPermissions(1, 15)"
                        type="primary"
                        >确认</a-button
                      >
                      <a-button
                        @click="handleResetPermissionFilter('resourceName')"
                        >重置</a-button
                      >
                    </a-space>
                  </div>
                </div>
              </template>
              <template #permission-name-filter>
                <div class="permission-filter">
                  <a-input
                    placeholder="输入权限名称进行搜索"
                    v-model="authorizeSearchKeywords.permissionName"
                  />
                  <div class="filter-footer">
                    <a-space>
                      <a-button
                        @click="handleGetUserGroupPermissions(1, 15)"
                        type="primary"
                        >确认</a-button
                      >
                      <a-button
                        @click="handleResetPermissionFilter('permissionName')"
                        >重置</a-button
                      >
                    </a-space>
                  </div>
                </div>
              </template>
              <template #permission-code-filter>
                <div class="permission-filter">
                  <a-input
                    placeholder="输入权限标识进行搜索"
                    v-model="authorizeSearchKeywords.permissionCode"
                  />
                  <div class="filter-footer">
                    <a-space>
                      <a-button
                        @click="handleGetUserGroupPermissions(1, 15)"
                        type="primary"
                        >确认</a-button
                      >
                      <a-button
                        @click="handleResetPermissionFilter('permissionCode')"
                        >重置</a-button
                      >
                    </a-space>
                  </div>
                </div>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>

    <!-- 添加用户对话框 -->
    <a-modal
      :visible="addGroupUserModalVisible"
      @cancel="handleCloseAddGroupUserModal"
      :footer="false"
      :width="728"
    >
      <template #title>添加成员</template>
      <div class="select-user-container">
        <div class="select-user-source">
          <a-input-search
            class="search"
            placeholder="输入用户名搜索成员"
            allow-clear
            v-model="searchSelectUserKeyword"
            @search="handleSearchUser"
            @keyup.enter.native="handleSearchUser"
            @clear="handleSearchUser"
          />
          <div class="check-all-container" v-if="allUsers.length > 0">
            <a-checkbox
              style="width: 100%"
              :model-value="selectUserCheckAll"
              :indeterminate="selectUserIndeterminate"
              @change="handleChangeCheckAll"
              >全选</a-checkbox
            >
          </div>
          <a-empty v-if="allUsers.length === 0" />
          <div
            class="scroll-container"
            v-else
            ref="allUsersContainerRef"
            @scroll="handleAllUsersContainerScroll"
          >
            <a-checkbox-group
              direction="vertical"
              style="width: 100%"
              v-model="addGroupUsersForm.userIds"
              @change="handleSelectUserChange"
            >
              <div class="select-user-item" v-for="(user, index) in allUsers">
                <a-checkbox
                  :key="index"
                  :value="user.userId"
                  style="width: 100%"
                >
                  {{ user.username }}
                </a-checkbox>
              </div>
            </a-checkbox-group>
          </div>
        </div>
        <div class="select-user-result">
          <div class="select-user-result-title">
            <span>已选：{{ addGroupUsersForm.userIds.length }} 名成员</span>
            <a-button type="text" size="mini" @click="handleClearSelctedUsers"
              >清空</a-button
            >
          </div>
          <div
            class="empty-container"
            v-if="addGroupUsersForm.userIds.length === 0"
          >
            <a-empty />
          </div>
          <div class="scroll-container" v-else>
            <div
              class="selected-user-item"
              v-for="(user, index) in slectedUsers"
              :key="index"
            >
              <div>
                {{ user.username }}
              </div>
              <div
                class="remove-container"
                @click="handleRemoveSelectedUser(user.userId)"
              >
                <icon-close />
              </div>
            </div>
          </div>
        </div>
      </div>
      <div class="operation-container">
        <a-space>
          <a-button @click="handleCloseAddGroupUserModal">取消</a-button>
          <a-button
            type="primary"
            :loading="addGroupUsersFormSubmitLoading"
            @click="handleAddGroupUsersFormSubmit"
            >确定</a-button
          >
        </a-space>
      </div>
    </a-modal>

    <authorize :visible="authorizeVisible" @close="authorizeVisible = false" />
  </div>
</template>
