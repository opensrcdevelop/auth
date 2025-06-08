<script lang="ts">
import createTs from "./index";
export default createTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="create-tile">创建用户字段</div>
      <div class="info-title">用户字段信息</div>
    </page-header>
    <a-form
      :model="createUserColumnForm"
      ref="createUserColumnFormRef"
      :rules="createUserColumnFormRules"
      layout="vertical"
      @submit-success="handleCreateUserColumnFormSubmit"
    >
      <a-row :gutter="24">
        <a-col :span="12">
          <a-form-item field="name" label="字段名称">
            <a-input
              v-model="createUserColumnForm.name"
              placeholder="请输入字段名称"
            />
          </a-form-item>
        </a-col>
        <a-col :span="12">
          <a-form-item field="key" label="字段 Key" tooltip="创建后不可以修改">
            <a-input
              v-model="createUserColumnForm.key"
              placeholder="请输入字段 Key"
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
              v-model="createUserColumnForm.dataType"
              placeholder="请选择表单类型"
              @change="handleDataTypeChange"
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
              v-model="createUserColumnForm.extFlg"
              placeholder="请选择字段类型"
            >
              <a-option :value="true">扩展字段</a-option>
              <a-option :value="false">基础字段</a-option>
            </a-select>
          </a-form-item>
        </a-col>
        <a-col :span="12">
          <a-form-item field="userLstDisplay" label="是否在用户列表显示">
            <a-select
              v-model="createUserColumnForm.userLstDisplay"
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
              v-model="createUserColumnForm.displayWidth"
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
              v-model="createUserColumnForm.userVisible"
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
              v-model="createUserColumnForm.userEditable"
              placeholder="请选择用户是否可编辑"
            >
              <a-option :value="true">可编辑</a-option>
              <a-option :value="false">不可编辑</a-option>
            </a-select>
          </a-form-item>
        </a-col>
        <a-col :span="12" v-if="createUserColumnForm.dataType === 'DICT'">
          <a-form-item
            field="dictId"
            label="字典类型"
            tooltip="创建后不可以修改"
          >
            <a-select
              v-model="createUserColumnForm.dictId"
              placeholder="请选择字典类型"
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
          <a-button type="primary" html-type="submit">创建</a-button>
          <a-button @click="handleResetCreateUserColumnForm">重置</a-button>
        </a-space>
      </a-form-item>
    </a-form>
  </div>
</template>
