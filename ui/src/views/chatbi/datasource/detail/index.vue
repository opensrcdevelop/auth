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
          <span class="title">{{ dataSourceName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="dataSourceId" textColor="#86909c" />
          </div>
        </div>
        <a-button type="primary" @click="hanleTestConn()">测试连接</a-button>
      </div>
    </page-header>
    <a-tabs :active-key="activeTab" @change="handleTabChange">
      <a-tab-pane key="data_source_info" title="数据源信息">
        <div class="tab-container">
          <div class="info-title">基本信息</div>
          <a-form
            :model="dataSourceInfoForm"
            layout="vertical"
            ref="dataSourceInfoFormRef"
            :rules="dataSourceInfoFormRules"
            @submit-success="handleDataSourceInfoFormSubmit"
          >
            <a-row :gutter="24">
              <a-col :span="12">
                <a-form-item field="name" label="数据源名称">
                  <a-input
                    v-model="dataSourceInfoForm.name"
                    placeholder="请输入数据源名称"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="type" label="数据源类型">
                  <a-select v-model="dataSourceInfoForm.type" disabled>
                    <a-option
                      v-for="item of dataSourceTypeList"
                      :value="item.value"
                      :label="item.label"
                    />
                  </a-select>
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="database" label="数据库">
                  <a-input disabled v-model="dataSourceInfoForm.database" />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="schema" label="模式">
                  <a-input disabled v-model="dataSourceInfoForm.schema" />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="host" label="主机地址">
                  <a-input
                    v-model="dataSourceInfoForm.host"
                    placeholder="请输入主机地址"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="port" label="端口号">
                  <a-input-number
                    v-model="dataSourceInfoForm.port"
                    :min="0"
                    hide-button
                    placeholder="请输入端口号"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="username" label="用户名">
                  <a-input
                    v-model="dataSourceInfoForm.username"
                    placeholder="请输入用户名"
                  />
                </a-form-item>
              </a-col>
              <a-col :span="12">
                <a-form-item field="password" label="密码">
                  <a-input-password
                    v-model="dataSourceInfoForm.password"
                    placeholder="请输入密码"
                  />
                </a-form-item>
              </a-col>
            </a-row>
            <a-form-item field="jdbcParams" label="JDBC 参数">
              <a-input
                v-model="dataSourceInfoForm.jdbcParams"
                placeholder="请输入 JDBC 参数"
              />
            </a-form-item>
            <a-form-item field="desc" label="数据源描述">
              <a-textarea
                v-model="dataSourceInfoForm.desc"
                placeholder="请输入数据源描述"
                :auto-size="{
                  minRows: 3,
                  maxRows: 5,
                }"
              />
            </a-form-item>
            <a-form-item hide-label>
              <a-space>
                <a-button type="primary" html-type="submit">保存</a-button>
                <a-button @click="handleResetDataSourceInfoForm">重置</a-button>
              </a-space>
            </a-form-item>
          </a-form>
        </div>
      </a-tab-pane>
      <a-tab-pane key="table_list" title="表列表">
        <div class="tab-container">
          <div class="table-list-header">
            <a-input-search
              :style="{ width: '320px' }"
              placeholder="输入表名称进行搜索"
              allow-clear
              v-model="tableSearchKeyword"
              @search="handleGetTableList(dataSourceId, 1, 15)"
              @keyup.enter.native="handleGetTableList(dataSourceId, 1, 15)"
              @clear="handleGetTableList(dataSourceId, 1, 15)"
            />
            <a-button
              type="primary"
              :disabled="saveBtnDisabled"
              @click="handleSaveTableList"
              >保存</a-button
            >
          </div>
          <a-table
            class="table-list"
            :data="tableList"
            :bordered="false"
            :scroll="{ y: '100%' }"
            :pagination="tableListPagination.pagination"
            @page-change="handleTableListPageChange"
            @page-size-change="handleTableListPageSizeChange"
          >
            <template #columns>
              <a-table-column title="编辑状态" :width="100">
                <template #cell="{ record }">
                  <div class="edit-status-container">
                    <transition name="fade">
                      <a-tag
                        v-if="!record._isHovering || !isRowModified(record)"
                        class="status-tag"
                        :class="{ modified: isRowModified(record) }"
                        @mouseenter="handleHoverIn(record)"
                      >
                        {{ isRowModified(record) ? "已编辑" : "未编辑" }}
                      </a-tag>
                    </transition>
                    <transition name="fade">
                      <a-button
                        v-if="record._isHovering && isRowModified(record)"
                        class="status-btn"
                        type="outline"
                        size="mini"
                        status="warning"
                        @mouseleave="handleHoverOut(record)"
                        @click="handleResetRow(record)"
                      >
                        还原
                      </a-button>
                    </transition>
                  </div>
                </template>
              </a-table-column>
              <a-table-column
                title="表名称"
                ellipsis
                tooltip
                :sortable="{
                  sortDirections: ['ascend', 'descend'],
                }"
              >
                <template #cell="{ record }">
                  <span
                    class="table-column-name"
                    @click="handleOpenTableFieldListDrawer(record)"
                  >
                    {{ record.name }}
                  </span>
                </template>
              </a-table-column>
              <a-table-column title="表注释">
                <template #cell="{ record }">
                  <a-space>
                    <span class="ellipsis-text">
                      {{ record.remark ? record.remark : "-" }}
                    </span>
                    <a-button
                      shape="circle"
                      size="mini"
                      @click="handleOpenTextEditorModal(record)"
                    >
                      <template #icon>
                        <icon-edit />
                      </template>
                    </a-button>
                  </a-space>
                </template>
              </a-table-column>
              <a-table-column title="补充信息">
                <template #cell="{ record }">
                  <a-space>
                    <span class="ellipsis-text">
                      {{ record.additionalInfo ? record.additionalInfo : "-" }}
                    </span>
                    <a-button
                      shape="circle"
                      size="mini"
                      @click="handleOpenMdEditorModal(record)"
                    >
                      <template #icon>
                        <icon-edit />
                      </template>
                    </a-button>
                  </a-space>
                </template>
              </a-table-column>
              <a-table-column title="是否使用" :width="100">
                <template #cell="{ record }">
                  <a-switch type="round" size="small" v-model="record.toUse" />
                </template>
              </a-table-column>
            </template>
          </a-table>
        </div>
      </a-tab-pane>
    </a-tabs>

    <TextEditorModal
      :visible="textEditorModalVisible"
      :content="textEditorModalContent"
      :title="textEditorModalTitle"
      @close="handleCloseTextEditorModal"
      @confirm="handleTextEditorModalConfirm"
    />

    <MdEditorModal
      :visible="mdEditorModalVisible"
      :content="mdEditorModalContent"
      :title="mdEditorModalTitle"
      @close="handleCloseMdEditorModal"
      @confirm="handleMdEditorModalConfirm"
    />

    <TableFieldListDrawer
      :visible="tableFieldListDrawerVisible"
      :tableId="tableFieldListDrawerTableId"
      :title="tableFieldListDrawerTitle"
      @close="handleCloseTableFieldListDrawer"
    />
  </div>
</template>
