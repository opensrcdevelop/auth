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
          <span class="title">{{ resourceName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="resourceId" textColor="#86909c" />
          </div>
        </div>
        <a-button type="primary" @click="handleToCreatePermission"
          >创建权限</a-button
        >
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="resource_info" title="资源信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="resourceInfoForm"
              ref="resourceInfoFormRef"
              :rules="resourceInfoFormRuels"
              layout="vertical"
              @submit-success="handleResourceInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="资源名称">
                    <a-input
                      v-model="resourceInfoForm.name"
                      placeholder="请输入资源名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="name" label="资源标识">
                    <a-input
                      v-model="resourceInfoForm.code"
                      placeholder="请输入资源标识"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="api" label="资源路径">
                <a-input
                  v-model="resourceInfoForm.api"
                  placeholder="请输入资源路径（URL）"
                />
              </a-form-item>
              <a-form-item field="desc" label="资源描述">
                <a-textarea
                  v-model="resourceInfoForm.desc"
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
                  <a-button @click="handleResetResourceInfoForm">重置</a-button>
                </a-space>
              </a-form-item>
            </a-form>
            <div class="info-title">所属资源组</div>
            <a-form :model="resourceGroupInfo" layout="vertical">
              <a-row :gutter="24">
                <a-col :span="8">
                  <a-form-item label="资源组ID">
                    <copy-text :text="resourceGroupInfo.id" />
                  </a-form-item>
                </a-col>
                <a-col :span="8">
                  <a-form-item label="资源组名称">
                    <a-input v-model="resourceGroupInfo.name" readonly />
                  </a-form-item>
                </a-col>
                <a-col :span="8">
                  <a-form-item label="资源组标识">
                    <a-input v-model="resourceGroupInfo.code" readonly />
                  </a-form-item>
                </a-col>
              </a-row>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="permission_list" title="权限列表">
          <div class="tab-container">
            <a-input-search
              :style="{ width: '320px' }"
              placeholder="输入权限名称或标识进行搜索"
              allow-clear
              v-model="permissionSearchKeyword"
              @search="handleSearchResourcePermissions"
              @keyup.enter.native="handleSearchResourcePermissions"
              @clear="handleSearchResourcePermissions"
            />
            <div class="permission-list">
              <a-table
                :data="permissions"
                :bordered="false"
                :pagination="permissionsPagination"
                :scroll="{ y: '100%' }"
                @pageChange="handlePageChange"
                @pageSizeChange="handlePageSizeChange"
              >
                <template #columns>
                  <a-table-column
                    title="权限名称"
                    ellipsis
                    tooltip
                    :sortable="{
                      sortDirections: ['ascend', 'descend'],
                    }"
                  >
                    <template #cell="{ record }">
                      <span
                        class="table-column-permissionname"
                        @click="handleToPermissionDetail(record)"
                      >
                        {{ record.permissionName }}
                      </span>
                    </template>
                  </a-table-column>
                  <a-table-column
                    title="权限标识"
                    ellipsis
                    tooltip
                    :sortable="{
                      sortDirections: ['ascend', 'descend'],
                    }"
                  >
                    <template #cell="{ record }">
                      {{ record.permissionCode }}
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
                            @click="handleDeletePermission(record)"
                          >
                            <template #icon>
                              <icon-delete />
                            </template>
                            删除</a-doption
                          >
                        </template>
                      </a-dropdown>
                    </template>
                  </a-table-column>
                </template>
              </a-table>
            </div>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>
  </div>
</template>
