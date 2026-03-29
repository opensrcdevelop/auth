<template>
  <div class="sample-sql-management">
    <!-- 嵌入模型配置 -->
    <div class="section-header">
      <div class="title">嵌入配置</div>
    </div>
    <div class="config">
      <a-form
        :model="embeddingConfigform"
        layout="vertical"
        :rules="embeddingConfigformRules"
        @submit-success="saveConfig"
      >
        <a-row :gutter="24">
          <a-col :span="6">
            <a-form-item field="providerId" label="模型提供商">
              <a-select
                v-model="embeddingConfigform.providerId"
                placeholder="请选择模型提供商"
                allow-clear
              >
                <a-option
                  v-for="provider in providerList"
                  :key="provider.id"
                  :value="provider.id"
                >
                  {{ provider.name }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
          <a-col :span="6">
            <a-form-item field="model" label="嵌入模型名称">
              <a-input
                v-model="embeddingConfigform.model"
                placeholder="请输入嵌入模型名称"
              />
            </a-form-item>
          </a-col>
          <a-col :span="4">
            <a-form-item field="dimension" label="嵌入模型维度">
              <a-input-number
                v-model="embeddingConfigform.dimension"
                :min="1"
              />
            </a-form-item>
          </a-col>
          <a-col :span="4">
            <a-form-item field="similarityThreshold" label="相似度阈值">
              <a-input-number
                v-model="embeddingConfigform.similarityThreshold"
                :min="0"
                :max="1"
                :step="0.1"
              />
            </a-form-item>
          </a-col>
          <a-col :span="4">
            <a-form-item field="topK" label="TopK">
              <a-input-number
                v-model="embeddingConfigform.topK"
                :min="1"
                :max="100"
                placeholder="返回结果数"
              />
            </a-form-item>
          </a-col>
        </a-row>
        <a-form-item hide-label>
          <a-button type="primary" html-type="submit">保存</a-button>
        </a-form-item>
      </a-form>
    </div>

    <!-- 示例 SQL -->
    <div class="section-header">
      <div class="title">示例 SQL</div>
    </div>
    <div class="toolbar">
      <div class="search-bar">
        <a-select
          v-model="searchForm.dataSourceId"
          placeholder="请选择数据源"
          style="width: 200px"
          allow-clear
          @change="handleDataSourceChange"
        >
          <a-option v-for="ds in dataSourceList" :key="ds.id" :value="ds.id">
            {{ ds.name }}
          </a-option>
        </a-select>
        <a-input-search
          v-model="searchForm.question"
          :placeholder="searchForm.useSimilarity ? '输入问题进行相似检索' : '输入问题进行过滤'"
          allow-clear
          :style="{ width: '260px' }"
          @search="handleSearch"
          @press-enter="handleSearch"
        />
        <a-checkbox v-model="searchForm.useSimilarity">
          相似检索
        </a-checkbox>
      </div>
      <div class="actions">
        <a-space>
          <a-button type="primary" @click="showAddModal = true">添加示例</a-button>
          <a-dropdown>
            <a-button>
              更多
            </a-button>
            <template #content>
              <a-doption @click="syncFromLikes">从 Likes 同步</a-doption>
              <a-doption @click="rebuild">重建索引</a-doption>
            </template>
          </a-dropdown>
        </a-space>
      </div>
    </div>
    <div class="table-container">
      <a-table
        :columns="columns"
        :data="tableData"
        :bordered="false"
        :pagination="tablePagination"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
      >
        <template #dataSource="{ record }">
          {{ getDataSourceName(record.dataSourceId) }}
        </template>
        <template #question="{ record }">
          <a-popover
            :title="'问题内容'"
            trigger="click"
            placement="left"
            :content-style="{ maxWidth: '600px', maxHeight: '400px', overflow: 'auto' }"
          >
            <template #content>
              <div class="question-content">
                <pre>{{ record.question }}</pre>
              </div>
              <a-button
                size="mini"
                :style="{ marginTop: '8px' }"
                @click="copySql(record.question)"
              >
                复制
              </a-button>
            </template>
            <a-button type="text" size="small" class="text-cell">
              <span class="text">{{ record.question }}</span>
            </a-button>
          </a-popover>
        </template>
        <template #sql="{ record }">
          <a-popover
            :title="'SQL 内容'"
            trigger="click"
            placement="left"
            :content-style="{ maxWidth: '600px', maxHeight: '400px', overflow: 'auto' }"
          >
            <template #content>
              <div class="sql-content">
                <pre>{{ record.sql }}</pre>
              </div>
              <a-button
                size="mini"
                :style="{ marginTop: '8px' }"
                @click="copySql(record.sql)"
              >
                复制
              </a-button>
            </template>
            <a-button type="text" size="small" class="sql-cell">
              <span class="sql-text">{{ record.sql }}</span>
            </a-button>
          </a-popover>
        </template>
        <template #score="{ record }">
          {{ record.score ? record.score.toFixed(2) : '-' }}
        </template>
        <template #optional="{ record }">
          <a-button type="text" status="danger" size="small" @click="openDeleteModal(record.id)">
            删除
          </a-button>
        </template>
      </a-table>
    </div>

    <AddSampleSqlModal v-model:visible="showAddModal" :data-source-list="dataSourceList" @success="loadData" />
  </div>
</template>

<script setup lang="ts">
import {computed, onMounted, onUnmounted, reactive, ref} from "vue";
import {Message, Modal, Notification} from "@arco-design/web-vue";
import {
  deleteSampleSql,
  getDataSourceConfList,
  getEmbeddingConfig,
  getEnabledModelProvider,
  getSampleSqlList,
  rebuildSampleSqlIndex,
  syncSampleSqlFromLikes,
  updateEmbeddingConfig,
} from "@/api/chatbi";
import AddSampleSqlModal from "./components/AddSampleSqlModal.vue";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {taskEmitter} from "@/hooks/taskEmitter";

const columns = [
  { title: "数据源", dataIndex: "dataSourceId", width: 150, slotName: "dataSource" },
  { title: "问题", dataIndex: "question", ellipsis: true, slotName: "question" },
  { title: "SQL", dataIndex: "sql", ellipsis: true, slotName: "sql" },
  { title: "相似度", dataIndex: "score", width: 100, slotName: "score" },
  { title: "创建时间", dataIndex: "createdAt", width: 200 },
  { title: "操作", slotName: "optional", width: 80 },
];

const showAddModal = ref(false);
const tableData = ref([]);
const dataSourceList = ref([]);
const providerList = ref([]);

// 根据 dataSourceId 获取数据源名称
const getDataSourceName = (dataSourceId: string) => {
  const ds = dataSourceList.value.find((item: any) => item.id === dataSourceId);
  return ds?.name || dataSourceId;
};

const pagination = ref({
  current: 1,
  pageSize: 15,
  total: 0,
  showTotal: true,
  showPageSize: true,
  pageSizeOptions: [15, 25, 50],
});

// 相似检索时只显示1页，分页控件禁用
const tablePagination = computed(() => {
  if (searchForm.useSimilarity) {
    return {
      ...pagination.value,
      current: 1,
      disabled: true,
    };
  }
  return pagination.value;
});

const searchForm = reactive({
  dataSourceId: undefined,
  question: undefined,
  useSimilarity: true,
});

const embeddingConfigform = reactive({
  providerId: undefined,
  model: undefined,
  similarityThreshold: 0.7,
  dimension: undefined,
  topK: 10,
});

const embeddingConfigformRules = {
  providerId: [{ required: true, message: "模型提供商未选择" }],
  model: [{ required: true, message: "嵌入模型名称未填写" }],
  similarityThreshold: [{ required: true, message: "相似度阈值未填写" }],
  dimension: [{ required: true, message: "嵌入模型维度未填写" }],
  topK: [{ required: true, message: "返回结果数未填写" }],
};

const loadData = () => {
  const searchType = searchForm.useSimilarity ? "similarity" : "simple";
  getSampleSqlList({
    dataSourceId: searchForm.dataSourceId,
    question: searchForm.question,
    searchType,
    current: pagination.value.current,
    size: pagination.value.pageSize,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      tableData.value = data?.list || [];
      pagination.value.total = data?.total || 0;
    });
  });
};

const loadConfig = () => {
  getDataSourceConfList({ page: -1, size: 15 })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dataSourceList.value = data?.list || [];
      });
    })
    .catch((err: any) => handleApiError(err, "获取数据源"));

  getEnabledModelProvider()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        providerList.value = data || [];
      });
    })
    .catch((err: any) => handleApiError(err, "获取模型提供商"));

  getEmbeddingConfig()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        embeddingConfigform.providerId = data?.providerId;
        embeddingConfigform.model = data?.model;
        embeddingConfigform.similarityThreshold = data?.similarityThreshold;
        embeddingConfigform.dimension = data?.dimension;
        embeddingConfigform.topK = data?.topK || 10;
      });
    })
    .catch((err: any) => handleApiError(err, "获取示例 SQL 嵌入配置"));
};

const saveConfig = async () => {
  try {
    const res = await updateEmbeddingConfig(embeddingConfigform);
    if (res?.data) {
      Notification.info("配置已保存，索引重建任务已提交，请前往任务中心查看进度")
    } else {
      Notification.success("保存成功");
    }
  } catch (e) {
    handleApiError(e, "保存示例 SQL 嵌入配置")
  }
};

const handleSearch = async () => {
  pagination.value.current = 1;
  pagination.value.total = 0;
  loadData();
};

const handleDataSourceChange = () => {
  pagination.value.current = 1;
  pagination.value.total = 0;
  loadData();
};

const handlePageChange = (page: number) => {
  pagination.value.current = page;
  loadData();
};

const handlePageSizeChange = (pageSize: number) => {
  pagination.value.pageSize = pageSize;
  pagination.value.current = 1;
  loadData();
};

const openDeleteModal = (id: string) => {
  Modal.confirm({
    title: "确定删除该示例 SQL 吗？",
    content: "此操作将不可恢复，请谨慎操作。",
    okButtonProps: {
      status: "danger",
    },
    onOk: async () => {
      try {
        await deleteSampleSql(id);
        Notification.success("删除成功");
        loadData();
      } catch (e) {
        handleApiError(e, "删除示例 SQL");
      }
    },
  });
};

const syncFromLikes = async () => {
  try {
    await syncSampleSqlFromLikes();
    Notification.info("任务已提交，请前往任务中心查看进度");
  } catch (e) {
    handleApiError(e, "从 Likes 同步示例 SQL");
  }
};

const rebuild = async () => {
  try {
    await rebuildSampleSqlIndex();
    Notification.info("任务已提交，请前往任务中心查看进度");
  } catch (e) {
    handleApiError(e, "重建示例 SQL 索引");
  }
};

const copySql = async (content: string) => {
  try {
    await navigator.clipboard.writeText(content);
    Message.success("复制成功");
  } catch (e) {
    Message.error("复制失败");
  }
};

// 监听任务完成事件，刷新列表
const handleTaskUpdate = (message: any) => {
  if (message.taskType === "SAMPLE_SQL_SYNC" || message.taskType === "SAMPLE_SQL_REBUILD") {
    if (message.status === "SUCCESS" || message.status === "FAILED") {
      loadData();
    }
  }
};

onMounted(() => {
  loadData();
  loadConfig();
  taskEmitter.on("task:update", handleTaskUpdate);
});

onUnmounted(() => {
  taskEmitter.off("task:update", handleTaskUpdate);
});

defineExpose({
  loadData,
  loadConfig,
});
</script>

<style scoped>
.sample-sql-management {
  padding: 16px;
}
.section-header {
  margin-bottom: 16px;
}
.section-header .title {
  font-size: 20px;
  font-weight: 500;
  color: #1d2129;
}
.config {
  width: 100%;
  margin-bottom: 16px;
}
.config-item {
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.config-item .label {
  font-size: 14px;
  color: #4e5969;
}
.toolbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 12px;
}
.search-bar {
  display: flex;
  gap: 12px;
}
.table-container {
  margin-top: 12px;
}
.sql-cell, .text-cell {
  max-width: 300px;
}
.sql-text, .text {
  display: block;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.sql-content pre, .question-content pre {
  white-space: pre-wrap;
  word-wrap: break-word;
  margin: 0;
  font-family: monospace;
  font-size: 12px;
}
</style>
