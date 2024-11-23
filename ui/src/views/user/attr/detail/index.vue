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
          <span class="title">{{ userColumnName }}</span>
          <div class="id">
            <span>ID:</span>
            <copy-text :text="userColumnId" textColor="#86909c" />
          </div>
        </div>
      </div>
      <a-tabs :active-key="activeTab" @change="handleTabChange">
        <a-tab-pane key="user_column_info" title="用户字段信息">
          <div class="tab-container">
            <div class="info-title">基本信息</div>
            <a-form
              :model="userColumnInfoForm"
              ref="userColumnInfoFormRef"
              :rules="userColumnInfoFormRules"
              layout="vertical"
              @submit="handleUserColumnInfoFormSubmit"
            >
              <a-row :gutter="24">
                <a-col :span="12">
                  <a-form-item field="name" label="字段名称">
                    <a-input
                      v-model="userColumnInfoForm.name"
                      placeholder="请输入字段名称"
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item
                    field="key"
                    label="字段 Key"
                    tooltip="创建后不可以修改"
                  >
                    <a-input
                      v-model="userColumnInfoForm.key"
                      placeholder="请输入字段 Key"
                      disabled
                    />
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item
                    field="dataType"
                    label="表单类型"
                    tooltip="创建后不可以修改"
                  >
                    <a-select
                      v-model="userColumnInfoForm.dataType"
                      placeholder="请选择表单类型"
                      disabled
                    >
                      <a-option value="STRING">字符串</a-option>
                      <a-option value="BOOLEAN">布尔值</a-option>
                      <a-option value="NUMBER">数字</a-option>
                      <a-option value="DATETIME">日期时间</a-option>
                      <a-option value="DATE">日期</a-option>
                      <a-option value="DICT">字典</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item
                    field="extFlg"
                    label="字段类型"
                    tooltip="创建后不可以修改"
                  >
                    <a-select
                      v-model="userColumnInfoForm.extFlg"
                      placeholder="请选择字段类型"
                      disabled
                    >
                      <a-option :value="true">扩展字段</a-option>
                      <a-option :value="false">基础字段</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item
                    field="userLstDisplay"
                    label="是否在用户列表显示"
                  >
                    <a-select
                      v-model="userColumnInfoForm.userLstDisplay"
                      placeholder="请选择是否在用户列表显示"
                    >
                      <a-option :value="true">显示</a-option>
                      <a-option :value="false">不显示</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="displayWidth" label="用户列表显示宽度">
                    <a-input-number
                      v-model="userColumnInfoForm.displayWidth"
                      placeholder="请输入用户列表显示宽度"
                      hide-button
                      :min="10"
                    >
                      <template #suffix>
                        <span>px</span>
                      </template>
                    </a-input-number>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="userVisible" label="个人中心是否可见">
                    <a-select
                      v-model="userColumnInfoForm.userVisible"
                      placeholder="请选择个人中心是否可见"
                    >
                      <a-option :value="true">可见</a-option>
                      <a-option :value="false">不可见</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12">
                  <a-form-item field="userEditable" label="用户是否可编辑">
                    <a-select
                      v-model="userColumnInfoForm.userEditable"
                      placeholder="请选择用户是否可编辑"
                    >
                      <a-option :value="true">可编辑</a-option>
                      <a-option :value="false">不可编辑</a-option>
                    </a-select>
                  </a-form-item>
                </a-col>
                <a-col :span="12" v-if="userColumnInfoForm.dataType === 'DICT'">
                  <a-form-item
                    field="dictId"
                    label="字典类型"
                    tooltip="创建后不可以修改"
                  >
                    <a-select
                      v-model="userColumnInfoForm.dictId"
                      placeholder="请选择字典类型"
                      disabled
                    >
                      <a-option
                        :value="dict.id"
                        v-for="dict in dictList"
                        :key="dict.id"
                        >{{ dict.name }}</a-option
                      >
                    </a-select>
                  </a-form-item>
                </a-col>
              </a-row>
              <a-form-item hide-label>
                <a-space>
                  <a-button type="primary" html-type="submit">保存</a-button>
                  <a-button @click="handleResetUserColumnInfoForm"
                    >重置</a-button
                  >
                </a-space>
              </a-form-item>
            </a-form>
          </div>
        </a-tab-pane>
      </a-tabs>
    </page-header>
  </div>
</template>
