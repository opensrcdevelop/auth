<template>
  <div v-if="message.type === 'TABLE'" class="table-container">
    <a-tabs v-model:active-key="activeTab" class="table-tabs">
      <a-tab-pane key="table" title="表格视图">
        <div class="table-actions">
          <a-button
            v-if="message.content.data && message.content.data.length > 0"
            type="text"
            size="mini"
            @click="handleDownloadCsv"
          >
            <template #icon>
              <icon-download />
            </template>
            下载 CSV
          </a-button>
        </div>
        <a-table
          column-resizable
          stripe
          :columns="message.content.columns"
          :data="message.content.data"
        />
      </a-tab-pane>
      <a-tab-pane key="sql" title="SQL 视图">
        <div class="sql-actions">
          <a-button type="text" size="mini" @click="handleCopySql">
            <template #icon>
              <icon-copy />
            </template>
            复制 SQL
          </a-button>
        </div>
        <div class="sql-editor">
          <MonacoEditor
            :model-value="message.content.sql"
            language="sql"
            :editor-option="editorOptions"
            height="200"
          />
        </div>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>

<script setup lang="ts">
import {ref} from 'vue';
import {Message} from '@arco-design/web-vue';
import MonacoEditor from '@/components/MonacoEditor.vue';

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const activeTab = ref('table');

const editorOptions = {
  readOnly: true,
  minimap: { show: false },
  scrollBeyondLastLine: false,
  lineNumbers: 'on' as const,
  folding: true,
  fontSize: 12,
  automaticLayout: true,
};

const handleCopySql = async () => {
  if (!props.message.content?.sql) {
    return;
  }
  try {
    await navigator.clipboard.writeText(props.message.content.sql);
    Message.success('复制成功');
  } catch {
    Message.error('复制失败');
  }
};

const handleDownloadCsv = () => {
  const { columns, data } = props.message.content;
  if (!data || data.length === 0) {
    return;
  }

  // 构建 CSV
  const headers = columns.map((col: any) => col.title).join(',');
  const rows = data.map((row: any) =>
    columns.map((col: any) => {
      const value = row[col.dataIndex];
      // 处理包含逗号或引号的值
      if (typeof value === 'string' && (value.includes(',') || value.includes('"'))) {
        return `"${value.replace(/"/g, '""')}"`;
      }
      return value ?? '';
    }).join(',')
  );
  const csv = [headers, ...rows].join('\n');

  // 下载
  const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = `data_export_${Date.now()}.csv`;
  link.click();
  URL.revokeObjectURL(url);
};
</script>

<style scoped lang="scss">
.table-container {
  background-color: #fff;
  border-radius: 8px;
  margin-top: 4px;
  padding: 16px;
  width: 100%;

  .table-tabs {
    :deep(.arco-tabs-nav) {
      margin-bottom: 12px;
    }
  }

  .table-actions,
  .sql-actions {
    display: flex;
    justify-content: flex-end;
    margin-bottom: 8px;
  }

  .sql-editor {
    border: 1px solid var(--color-border);
    border-radius: 4px;
    overflow: hidden;
  }
}
</style>
