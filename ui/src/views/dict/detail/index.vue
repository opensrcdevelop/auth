<script lang="ts">
import indexTs from "./index";
export default indexTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-header">
        <div>
          <span class="title">{{ dictName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="dictId" textColor="#86909c" />
          </div>
        </div>
        <a-button type="primary" @click="handleToCreateDictData"
          >创建字典数据</a-button
        >
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="dict_info" title="字典信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="dictInfoForm"
              ref="dictInfoFormRef"
              :rules="dictInfoFormRules"
              layout="vertical"
              @submit-success="handleDictInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="字典名称">
                    <a-input
                      v-model="dictInfoForm.name"
                      placeholder="请输入字典名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="code" label="字典标识">
                    <a-input
                      v-model="dictInfoForm.code"
                      placeholder="请输入字典标识"
                    />
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item field="desc" label="字典描述">
                <a-textarea
                  v-model="dictInfoForm.desc"
                  placeholder="请输入字典描述"
                  :auto-size="{
                    minRows: 3,
                    maxRows: 5,
                  }"
                />
              </a-form-item>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetDictInfoForm">重置</a-button>
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
        <a-tab-pane key="dict_data" title="数据列表">
          <div class="tab-container">
            <a-input-search
              v-model="dictDataSerachKeyword"
              :style="{ width: '320px' }"
              placeholder="输入数据标签或值进行搜索"
              allow-clear
              @search="handleGetDictDataList(1, 15)"
              @keyup.enter.native="handleGetDictDataList(1, 15)"
              @clear="handleGetDictDataList(1, 15)"
            />
            <div class="dict-data-list">
              <a-table
                :data="dictDataList"
                :bordered="false"
                :scroll="{ y: '100%' }"
                :pagination="dictDataPagination"
                @page-change="handlePageChange"
                @page-size-change="handlePageSizeChange"
              >
                <template #columns>
                  <a-table-column
                    title="数据标签"
                    ellipsis
                    tooltip
                    :sortable="{
                      sortDirections: ['ascend', 'descend'],
                    }"
                  >
                    <template #cell="{ record }">
                      <span
                        class="table-column-dictdatalabel"
                        @click="handleToDictDataDetail(record)"
                      >
                        {{ record.label }}
                      </span>
                    </template>
                  </a-table-column>
                  <a-table-column
                    title="数据值"
                    ellipsis
                    tooltip
                    :sortable="{
                      sortDirections: ['ascend', 'descend'],
                    }"
                  >
                    <template #cell="{ record }">
                      <span>
                        {{ record.value }}
                      </span>
                    </template>
                  </a-table-column>
                  <a-table-column
                    title="状态"
                    ellipsis
                    tooltip
                    :sortable="{
                      sortDirections: ['ascend', 'descend'],
                    }"
                  >
                    <template #cell="{ record }">
                      <span>
                        {{ record.enable ? "启用" : "禁用" }}
                      </span>
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
                            @click="handleDeleteDictData(record)"
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
