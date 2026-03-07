<template>
  <div class="sample-sql-management">
    <!-- 嵌入模型配置 -->
    <div class="section-header">
      <div class="title">嵌入配置</div>
    </div>
    <div class="config-bar">
      <div class="config-item">
        <div class="label">模型提供商</div>
        <a-select v-model="form.providerId" placeholder="请选择模型提供商" style="width: 200px" allow-clear>
          <a-option v-for="provider in providerList" :key="provider.id" :value="provider.id">
            {{ provider.name }}
          </a-option>
        </a-select>
      </div>
      <div class="config-item">
        <div class="label">嵌入模型名称</div>
        <a-input v-model="form.model" placeholder="如: text-embedding-ada-002" style="width: 300px" />
      </div>
      <div class="config-item">
        <div class="label">相似度阈值</div>
        <a-input-number v-model="form.similarityThreshold" :min="0" :max="1" :step="0.1" style="width: 100px" />
      </div>
      <a-button type="primary" @click="saveConfig">保存配置</a-button>
    </div>

    <!-- 示例 SQL -->
    <div class="section-header">
      <div class="title">示例 SQL</div>
    </div>
    <div class="toolbar">
      <div class="search-bar">
        <a-select v-model="searchForm.dataSourceId" placeholder="请选择数据源" style="width: 200px" allow-clear>
          <a-option v-for="ds in dataSourceList" :key="ds.id" :value="ds.id">
            {{ ds.name }}
          </a-option>
        </a-select>
        <a-input-search
          v-model="searchForm.question"
          placeholder="输入问题进行搜索"
          allow-clear
          search-button
          :style="{ width: '320px' }"
          @search="handleSearch"
        />
      </div>
      <div class="actions">
        <a-space>
          <a-button type="primary" @click="showAddModal = true">添加示例</a-button>
          <a-button @click="syncFromLikes">从 Likes 同步</a-button>
          <a-button @click="rebuild">重建索引</a-button>
        </a-space>
      </div>
    </div>
    <div class="table-container">
      <a-table
        :columns="columns"
        :data="tableData"
        :loading="loading"
        :bordered="false"
        :pagination="pagination"
        @page-change="handlePageChange"
        @page-size-change="handlePageSizeChange"
      >
        <template #optional="{ record }">
          <a-button type="text" status="danger" size="small" @click="handleDelete(record.id)">删除</a-button>
        </template>
      </a-table>
    </div>

    <AddSampleSqlModal v-model:visible="showAddModal" @success="loadData" />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, reactive } from 'vue';
import { Message } from '@arco-design/web-vue';
import { getDataSourceConfList, getEnabledModelProvider, getSampleSqlList, deleteSampleSql, syncSampleSqlFromLikes, rebuildSampleSqlIndex, getEmbeddingConfig, updateEmbeddingConfig } from '@/api/chatbi';
import AddSampleSqlModal from './components/AddSampleSqlModal.vue';

const columns = [
  { title: 'ID', dataIndex: 'id', ellipsis: true, width: 220 },
  { title: '数据源', dataIndex: 'dataSourceId', width: 150 },
  { title: '问题', dataIndex: 'question', ellipsis: true },
  { title: 'SQL', dataIndex: 'sql', ellipsis: true },
  { title: '相似度', dataIndex: 'score', width: 100 },
  { title: '创建时间', dataIndex: 'createdAt', width: 180 },
  { title: '操作', slotName: 'optional', width: 80 },
];

const loading = ref(false);
const showAddModal = ref(false);
const tableData = ref([]);
const dataSourceList = ref([]);
const providerList = ref([]);

const pagination = ref({
  current: 1,
  pageSize: 10,
  total: 0,
  showTotal: true,
  showPageSize: true,
});

const searchForm = reactive({
  dataSourceId: '',
  question: '',
});

const form = reactive({
  providerId: '',
  model: '',
  similarityThreshold: 0.7,
});

const loadData = async () => {
  loading.value = true;
  try {
    const res = await getSampleSqlList({
      current: pagination.value.current,
      size: pagination.value.pageSize,
    });
    const pageData = res.data?.data;
    tableData.value = pageData?.list || [];
    pagination.value.total = pageData?.total || 0;
  } finally {
    loading.value = false;
  }
};

const loadConfig = async () => {
  try {
    console.log('开始加载配置...');
    const dsRes = await getDataSourceConfList({ page: 1, size: 100 });
    console.log('dsRes:', dsRes);
    const providerRes = await getEnabledModelProvider();
    console.log('providerRes:', providerRes);
    const configRes = await getEmbeddingConfig();
    console.log('configRes:', configRes);

    if (dsRes.success) {
      dataSourceList.value = dsRes.data?.list || dsRes.data || [];
    }
    if (providerRes.success) {
      providerList.value = providerRes.data || [];
    }
    if (configRes.success && configRes.data) {
      form.providerId = configRes.data.providerId;
      form.model = configRes.data.model;
      form.similarityThreshold = configRes.data.similarityThreshold;
    }
  } catch (e) {
    console.error('加载配置失败', e);
  }
};

const saveConfig = async () => {
  try {
    await updateEmbeddingConfig(form);
    Message.success('保存成功');
  } catch (e) {
    Message.error('保存失败');
  }
};

const handleSearch = async () => {
  pagination.value.current = 1;
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

const handleDelete = async (id: string) => {
  try {
    await deleteSampleSql(id);
    Message.success('删除成功');
    loadData();
  } catch (e) {
    Message.error('删除失败');
  }
};

const syncFromLikes = async () => {
  try {
    const res = await syncSampleSqlFromLikes();
    Message.success(`同步成功，共 ${res.data} 条`);
    loadData();
  } catch (e) {
    Message.error('同步失败');
  }
};

const rebuild = async () => {
  try {
    const res = await rebuildSampleSqlIndex();
    Message.success(`重建成功，共 ${res.data} 条`);
    loadData();
  } catch (e) {
    Message.error('重建失败');
  }
};

onMounted(() => {
  loadData();
  loadConfig();
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
.config-bar {
  display: flex;
  align-items: flex-end;
  gap: 24px;
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
</style>
