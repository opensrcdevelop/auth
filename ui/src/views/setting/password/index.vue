<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="setting-header">
      <div class="left">
        <div class="title">密码安全</div>
        <div class="info">支持对不同主体配置差异化的密码策略。</div>
      </div>
      <a-button type="primary" @click="handleToCreatePasswordPolicy"
        >创建密码策略</a-button
      >
    </div>

    <div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="password_policy" title="密码策略">
          <div class="tab-container">
            <a-table
              :data="passwordPolicyList"
              :bordered="false"
              :scroll="{ y: '100%' }"
              :pagination="false"
              :draggable="{ type: 'handle', width: 20 }"
              @change="handlTableChange"
            >
              <template #columns>
                <a-table-column title="执行顺序" :width="100">
                  <template #cell="{ rowIndex }">
                    <span>{{ rowIndex + 1 }} </span>
                  </template>
                </a-table-column>
                <a-table-column title="策略名称" ellipsis tooltip>
                  <template #cell="{ record }">
                    <span
                      class="table-column-policyname"
                      @click="handleToPasswordPolicyDetail(record.id)"
                    >
                      {{ record.name }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="密码强度">
                  <template #cell="{ record }">
                    <a-tag color="gray">
                      {{ getPasswordStrengthLabel(record.passwordStrength) }}
                    </a-tag>
                  </template>
                </a-table-column>
                <a-table-column title="状态">
                  <template #cell="{ record }">
                    <a-switch
                      v-model="record.enabled"
                      @change="handleUpdatePasswordPolicyState(record)"
                      type="round"
                      size="small"
                    />
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
                          @click="handleDeletePasswordPolicy(record)"
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
        </a-tab-pane>
        <a-tab-pane key="remind_logs" title="密码到期提醒记录">
          <div class="tab-container">
            <a-input-search
              :style="{ width: '320px', marginBottom: '16px' }"
              placeholder="输入用户名或策略名称进行搜索"
              allow-clear
              v-model="remindLogSearchKeyword"
              @search="handleGetRemindLogList(1, 15)"
              @keyup.enter.native="handleGetRemindLogList(1, 15)"
              @clear="handleGetRemindLogList(1, 15)"
            />
            <a-table
              :data="remindLogList"
              :bordered="false"
              :scroll="{ y: '100%' }"
              :pagination="remindLogListPagination.pagination"
              @page-change="remindLogListPagination.handlePageChange"
              @page-size-change="remindLogListPagination.handlePageSizeChange"
            >
              <template #columns>
                <a-table-column title="用户">
                  <template #cell="{ record }">
                    <span class="table-column-username" @click="handleToUserDetail(record.userId)">
                      {{ record.username }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="密码策略">
                  <template #cell="{ record }">
                    <span class="table-column-policyname" @click="handleToPasswordPolicyDetail(record.policyId)">
                      {{ record.policyName }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="提醒时间">
                  <template #cell="{ record }">
                    <span>
                      {{ record.remindTime }}
                    </span>
                  </template>
                </a-table-column>
                <a-table-column title="提醒方式">
                  <template #cell="{ record }">
                    <span v-if="record.remindMethod === 'MAIL'"> 邮件 </span>
                  </template>
                </a-table-column>
                <a-table-column title="结果">
                  <template #cell="{ record }">
                    <a-tag color="green" v-if="record.success">成功</a-tag>
                    <a-tag color="red" v-else>失败</a-tag>
                  </template>
                </a-table-column>
              </template>
            </a-table>
          </div>
        </a-tab-pane>
      </a-tabs>
    </div>
  </div>
</template>
