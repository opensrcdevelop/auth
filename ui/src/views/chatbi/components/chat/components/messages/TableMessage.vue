<template>
  <div v-if="message.type === 'TABLE'" class="table-container">
    <a-collapse class="table-collapse" :bordered="false">
      <a-collapse-item key="table">
        <template #header>
          <span class="title">数据查询</span>
        </template>
        <div class="view-switch">
          <a-radio-group v-model="activeView" type="button" size="small">
            <a-radio value="table">表格视图</a-radio>
            <a-radio value="sql">SQL 视图</a-radio>
          </a-radio-group>
          <div class="view-actions">
            <a-button
              v-if="
                activeView === 'table' &&
                message.content.data &&
                message.content.data.length > 0
              "
              type="text"
              size="mini"
              @click="handleDownloadCsv"
            >
              <template #icon>
                <icon-download />
              </template>
              下载 CSV
            </a-button>
            <a-button
              v-if="activeView === 'sql'"
              type="text"
              size="mini"
              @click="handleCopySql"
            >
              <template #icon>
                <icon-copy />
              </template>
              复制 SQL
            </a-button>
          </div>
        </div>
        <a-table
          v-if="activeView === 'table'"
          column-resizable
          stripe
          :columns="message.content.columns"
          :data="message.content.data"
        />
        <div v-if="activeView === 'sql'" class="sql-editor">
          <MonacoEditor
            :model-value="message.content.sql"
            language="sql"
            :editor-option="editorOptions"
            :height="200"
          />
        </div>
      </a-collapse-item>
    </a-collapse>
  </div>
</template>

<script setup lang="ts">
import {ref} from "vue";
import {Message} from "@arco-design/web-vue";
import MonacoEditor from "@/components/MonacoEditor.vue";
import {copyToClipboard} from "@/util/tool";

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  },
);

const activeView = ref("table");

const editorOptions = {
  readOnly: true,
  minimap: { show: false },
  scrollBeyondLastLine: false,
  lineNumbers: "on" as const,
  folding: true,
  fontSize: 12,
  automaticLayout: true,
};

const handleCopySql = async () => {
  if (!props.message.content?.sql) {
    return;
  }
  const result = await copyToClipboard(props.message.content.sql);
  if (result) {
    Message.success("复制成功");
  } else {
    Message.error("复制失败");
  }
};

const handleDownloadCsv = () => {
  const { columns, data } = props.message.content;
  if (!data || data.length === 0) {
    return;
  }

  // 构建 CSV
  const headers = columns.map((col: any) => col.title).join(",");
  const rows = data.map((row: any) =>
    columns
      .map((col: any) => {
        const value = row[col.dataIndex];
        // 处理包含逗号或引号的值
        if (
          typeof value === "string" &&
          (value.includes(",") || value.includes('"'))
        ) {
          return `"${value.replace(/"/g, '""')}"`;
        }
        return value ?? "";
      })
      .join(","),
  );
  const csv = [headers, ...rows].join("\n");

  // 下载
  const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = `data_export_${Date.now()}.csv`;
  link.click();
  URL.revokeObjectURL(url);
};
</script>

<style scoped lang="scss">
.table-container {
  margin-top: 4px;

  .title {
    color: var(--color-neutral-6);
  }

  .table-collapse {
    background-color: #fff;

    :deep(.arco-collapse-item) {
      background-color: #fff;
    }

    :deep(.arco-collapse-item-header) {
      background-color: #fff;
    }

    :deep(.arco-collapse-item-content) {
      background-color: #fff;
    }
  }

  .view-switch {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 12px;
  }

  .view-actions {
    display: flex;
    gap: 8px;
  }
}
</style>
