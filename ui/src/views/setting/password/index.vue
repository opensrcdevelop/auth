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
                      @click="handleToPasswordPolicyDetail(record)"
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
                        <a-doption style="color: #e8353e" @click="handleDeletePasswordPolicy(record)">
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
      </a-tabs>
    </div>
  </div>
</template>
