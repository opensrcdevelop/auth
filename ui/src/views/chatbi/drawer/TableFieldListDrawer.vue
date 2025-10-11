<template>
  <a-drawer
    :visible="visible"
    :footer="false"
    width="100%"
    @cancel="handleClose"
  >
    <template #title> {{ title }} </template>
    <a-spin
      class="spin"
      tip="处理中，请稍后..."
      :loading="globalVariables.apiLoading"
    >
      <div class="list-container">
        <div class="list-header">
          <a-input-search
            :style="{ width: '320px' }"
            placeholder="输入字段名称进行搜索"
            allow-clear
            v-model="tableFieldSearchKeyword"
            @search="handleGetTableFieldList(1, 15)"
            @keyup.enter.native="handleGetTableFieldList(1, 15)"
            @clear="handleGetTableFieldList(1, 15)"
          />
          <a-button
            type="primary"
            :disabled="saveBtnDisabled"
            @click="handleSaveTableFieldList"
            >保存</a-button
          >
        </div>
        <a-table
          :data="tableFieldList"
          :bordered="false"
          :scroll="{ y: '100%' }"
          :pagination="tableFieldListPagination.pagination"
          @page-change="handleTableFieldListPageChange"
          @page-size-change="handleTableFieldListPageSizeChange"
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
              title="字段名称"
              ellipsis
              tooltip
              :sortable="{
                sortDirections: ['ascend', 'descend'],
              }"
            >
              <template #cell="{ record }">
                <span>
                  {{ record.name }}
                </span>
              </template>
            </a-table-column>
            <a-table-column title="字段类型" :width="100">
              <template #cell="{ record }">
                <span>
                  {{ record.type }}
                </span>
              </template>
            </a-table-column>
            <a-table-column title="字段注释">
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
    </a-spin>
  </a-drawer>

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
</template>

<script setup lang="ts">
import {batchUpdateTableField, getTableFieldList} from "@/api/chatbi";
import {usePagination} from "@/hooks/usePagination";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {computed, reactive, ref, watch} from "vue";
import TextEditorModal from "../modal/TextEditorModal.vue";
import MdEditorModal from "../modal/MdEditorModal.vue";
import {Notification} from "@arco-design/web-vue";
import {useGlobalVariablesStore} from "@/store/globalVariables";

const globalVariables = useGlobalVariablesStore();

const props = withDefaults(
  defineProps<{
    visible: boolean;
    title: string;
    tableId: string;
  }>(),
  {
    visible: false,
    title: "",
    tableId: "",
  }
);

const emits = defineEmits<{
  (e: "close"): void;
}>();

const handleClose = () => {
  if (detectRowChanges()) {
    tableFieldList.length = 0;
    emits("close");
  }
};

watch(
  () => props.visible,
  (val) => {
    if (val) {
      handleGetTableFieldList(1, 15);
    }
  }
);

/** 表字段列表 */
const tableFieldList = reactive([]);
const tableFieldSearchKeyword = ref("");
const tableFieldListPagination = usePagination(
  "tableFieldList",
  ({ page, size }) => {
    handleGetTableFieldList(page, size);
  }
);

const handleTableFieldListPageChange = (page: number) => {
  tableFieldListPagination.handlePageChange(page, detectRowChanges);
};

const handleTableFieldListPageSizeChange = (size: number) => {
  tableFieldListPagination.handlePageSizeChange(size, detectRowChanges);
};

/**
 * 判断行数据是否被修改过
 */
const isRowModified = (rowData: any) => {
  if (!rowData._originalData) return false;

  const { _isHovering, _originalData, ...currentData } = rowData;
  return JSON.stringify(currentData) !== JSON.stringify(_originalData);
};

const detectRowChanges = () => {
  if (!saveBtnDisabled.value) {
    return confirm("有未保存的编辑，是否离开当前页面？");
  }
  return true;
};

/**
 * 鼠标悬停
 */
const handleHoverIn = (rowData: any) => {
  if (isRowModified(rowData)) {
    rowData._isHovering = true;
  }
};

/**
 * 鼠标移出
 */
const handleHoverOut = (rowData: any) => {
  if (isRowModified(rowData)) {
    rowData._isHovering = false;
  }
};

/**
 * 重置行数据
 */
const handleResetRow = (record: any) => {
  if (record._originalData) {
    const index = tableFieldList.findIndex((item) => item.id === record.id);
    if (index !== -1) {
      const originalData = JSON.parse(JSON.stringify(record._originalData));
      originalData._originalData = record._originalData;
      originalData._isHovering = false;
      tableFieldList[index] = originalData;
    }
  }
};

/** 保存按钮禁用状态 */
const saveBtnDisabled = computed(() => {
  if (!tableFieldList) return true;
  if (tableFieldList.find((item) => isRowModified(item))) {
    return false;
  }
  return true;
});

/**
 * 获取表字段列表
 */
const handleGetTableFieldList = (page: number = 1, size: number = 15) => {
  if (!props.tableId) return;
  getTableFieldList(props.tableId, {
    page,
    size,
    keyword: tableFieldSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        tableFieldList.length = 0;

        data.list.forEach((item) => {
          tableFieldList.push({
            ...item,
            _isHovering: false,
            _originalData: JSON.parse(JSON.stringify(item)),
          });
        });

        tableFieldListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取表字段列表");
    });
};

/** 表字段注释编辑对话框 */
const textEditorModalVisible = ref(false);
const textEditorModalContent = ref("");
const textEditorModalTitle = ref("");
const textEditorModalRecordId = ref("");

/**
 * 打开表字段注释编辑对话框
 */
const handleOpenTextEditorModal = (record: any) => {
  textEditorModalContent.value = record.remark;
  textEditorModalVisible.value = true;
  textEditorModalTitle.value = "编辑字段注释 - " + record.name;
  textEditorModalRecordId.value = record.id;
};

/**
 * 关闭表字段注释编辑对话框
 */
const handleCloseTextEditorModal = () => {
  textEditorModalVisible.value = false;
  textEditorModalContent.value = "";
  textEditorModalTitle.value = "";
  textEditorModalRecordId.value = "";
};

/**
 * 表字段注释编辑对话框确认
 */
const handleTextEditorModalConfirm = (newContent: string) => {
  const targetRecord = tableFieldList.find(
    (item) => item.id === textEditorModalRecordId.value
  );
  if (targetRecord) {
    targetRecord.remark = newContent;
  }
  handleCloseTextEditorModal();
};

/** 表字段补充信息编辑对话框 */
const mdEditorModalVisible = ref(false);
const mdEditorModalContent = ref("");
const mdEditorModalTitle = ref("");
const mdEditorModalRecordId = ref("");

/**
 * 打开表字段补充信息编辑对话框
 */
const handleOpenMdEditorModal = (record: any) => {
  mdEditorModalContent.value = record.additionalInfo;
  mdEditorModalVisible.value = true;
  mdEditorModalTitle.value = "编辑字段补充信息 - " + record.name;
  mdEditorModalRecordId.value = record.id;
};

/**
 * 关闭表字段补充信息编辑对话框
 */
const handleCloseMdEditorModal = () => {
  mdEditorModalVisible.value = false;
  mdEditorModalContent.value = "";
  mdEditorModalTitle.value = "";
  mdEditorModalRecordId.value = "";
};

/**
 * 表字段补充信息编辑对话框确认
 */
const handleMdEditorModalConfirm = (newContent: string) => {
  const targetRecord = tableFieldList.find(
    (item) => item.id === mdEditorModalRecordId.value
  );
  if (targetRecord) {
    targetRecord.additionalInfo = newContent;
  }
  handleCloseMdEditorModal();
};

/**
 * 保存表字段数据
 */
const handleSaveTableFieldList = () => {
  const list = tableFieldList
    .filter((item) => isRowModified(item))
    .map((item) => {
      const { _isHovering, _originalData, ...currentData } = item;
      return currentData;
    });
  batchUpdateTableField({
    list,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetTableFieldList();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "批量更新表字段");
    });
};
</script>

<style lang="scss" scoped>
.spin {
  width: 100%;
  height: 100%;
}

.list-container {
  width: 100%;
  height: calc(100% - 48px);
}

.list-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 16px;
}

.ellipsis-text {
  display: inline-block;
  max-width: 280px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.edit-status-container {
  position: relative;
  width: 100%;
  height: 32px;
  display: flex;
  align-items: center;
  justify-content: center;

  .status-tag,
  .status-btn {
    position: absolute;
    left: 0;
    right: 0;
    display: flex;
    justify-content: center;
  }
}

.fade-enter-active,
.fade-leave-active {
  transition: opacity 0.3s ease;
}

.fade-enter-from,
.fade-leave-to {
  opacity: 0;
}

.fade-enter-to,
.fade-leave-from {
  opacity: 1;
}
</style>
