<template>
  <div v-if="message.type === 'TABLE'" class="table-container">
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
    <div v-if="activeView === 'table'" class="view-container">
      <a-table
        column-resizable
        stripe
        :columns="message.content.columns"
        :data="message.content.data"
        :pagination="{
          showTotal: true,
          showJumper: true,
          pageSize: 5,
        }"
      />
    </div>
    <div v-if="activeView === 'sql'" class="view-container">
      <monaco-editor
        v-model="message.content.sql"
        language="sql"
        :editorOption="editorOptions"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import {ref} from "vue";
import {Message} from "@arco-design/web-vue";
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
  fontSize: 13,
  automaticLayout: true,
  contextmenu: false,
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
  background-color: #fff;
  padding: 16px;
  margin-top: 4px;
  margin-bottom: 16px;
  border-radius: 4px;
  height: 370px;
  overflow: auto;

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

  .view-container {
    height: 280px;
  }
}
</style>
