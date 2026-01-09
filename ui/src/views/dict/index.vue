<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="dict-header">
      <div class="left">
        <div class="title">数据字典</div>
        <div class="info">集中数据元素，提供统一管理。</div>
      </div>
      <a-button type="primary" @click="handleToCreateDict">创建字典</a-button>
    </div>
    <div class="dict-search">
      <a-input-search
        v-model="dictSerachKeyword"
        :style="{ width: '320px' }"
        placeholder="输入字典名称或标识进行搜索"
        allow-clear
        @search="handleGetDictList(1, 15)"
        @keyup.enter.native="handleGetDictList(1, 15)"
        @clear="handleGetDictList(1, 15)"
      />
    </div>
    <div class="dict-list">
      <a-table
        :data="dictList"
        :bordered="false"
        :scroll="{ y: '100%' }"
        :pagination="dictPagination.pagination"
        @page-change="dictPagination.handlePageChange"
        @page-size-change="dictPagination.handlePageSizeChange"
        row-key="uniqueKey"
      >
        <template #columns>
          <a-table-column
            title="字典名称"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span
                class="table-column-dictname"
                @click="handleToDictDetail(record)"
              >
                {{ record.name }}
              </span>
            </template>
          </a-table-column>
          <a-table-column
            title="字典标识"
            ellipsis
            tooltip
            :sortable="{
              sortDirections: ['ascend', 'descend'],
            }"
          >
            <template #cell="{ record }">
              <span>
                {{ record.code }}
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
                    style="color: var(--color-neutral-8)"
                    @click="handleAddChildDict(record.id)"
                  >
                    <template #icon>
                      <icon-plus-circle />
                    </template>
                    添加子字典
                  </a-doption>
                  <a-doption
                    v-if="record?.level > 1"
                    style="color: #e8353e"
                    @click="handleRemoveChildDict(record)"
                  >
                    <template #icon>
                      <icon-minus-circle />
                    </template>
                    移除子字典
                  </a-doption>
                  <a-doption
                    style="color: #e8353e"
                    @click="handleDeleteDict(record)"
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

    <a-modal
      v-model:visible="selectableChildDictModalVisible"
      :mask-closable="false"
      @ok="handleAddChildDictsFormSubmit"
      @cancel="handleCloseAddChildDictsModal"
    >
      <template #title> 添加子字典 </template>
      <div>
        <a-form
          :model="addChildDictsForm"
          :rules="addChildDictsFormRules"
          ref="addChildDictsFormRef"
          layout="vertical"
        >
          <a-form-item field="children" label="子字典">
            <a-select
              placeholder="请选择子字典"
              multiple
              v-model:model-value="addChildDictsForm.children"
              @change="(val) => addChildDictsFormRef.validate()"
            >
              <a-option
                v-for="child in selectableChildDictList"
                :key="child.id"
                :value="child.id"
                >{{ child.name }}</a-option
              >
            </a-select>
          </a-form-item>
          <a-form-item field="relatedDictDataIds" label="关联字典数据">
            <a-select
              placeholder="请选择关联的字典数据"
              multiple
              v-model:model-value="addChildDictsForm.relatedDictDataIds"
              @change="(val) => addChildDictsFormRef.validate()"
            >
              <a-option
                v-for="data in relatableDictDataList"
                :key="data.id"
                :value="data.id"
                >{{ data.label }}</a-option
              >
            </a-select>
          </a-form-item>
        </a-form>
      </div>
    </a-modal>
  </div>
</template>
