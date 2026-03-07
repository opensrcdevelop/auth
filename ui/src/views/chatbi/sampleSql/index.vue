<template>
  <div class="sample-sql-management">
    <a-card>
      <a-form :model="form" layout="inline">
        <a-form-item label="数据源">
          <a-select v-model="form.dataSourceId" placeholder="请选择数据源" style="width: 200px" allow-clear>
            <a-option v-for="ds in dataSourceList" :key="ds.dataSourceId" :value="ds.dataSourceId">
              {{ ds.dataSourceName }}
            </a-option>
          </a-select>
        </a-form-item>
        <a-form-item label="嵌入模型">
          <a-select v-model="form.providerId" placeholder="请选择嵌入模型" style="width: 200px">
            <a-option v-for="provider in providerList" :key="provider.providerId" :value="provider.providerId">
              {{ provider.providerName }}
            </a-option>
          </a-select>
        </a-form-item>
        <a-form-item label="相似度阈值">
          <a-input-number v-model="form.similarityThreshold" :min="0" :max="1" :step="0.1" style="width: 100px" />
        </a-form-item>
        <a-form-item>
          <a-button type="primary" @click="saveConfig">保存配置</a-button>
        </a-form-item>
      </a-form>
    </a-card>

    <a-card style="margin-top: 16px">
      <div class="action-bar">
        <a-space>
          <a-button type="primary" @click="showAddModal = true">添加示例</a-button>
          <a-button @click="syncFromLikes">从 Likes 同步</a-button>
          <a-button @click="rebuild">重建索引</a-button>
        </a-space>
      </div>

      <a-table :columns="columns" :data="tableData" :loading="loading" style="margin-top: 16px">
        <template #optional="{ record }">
          <a-button type="text" status="danger" size="small" @click="handleDelete(record.id)">删除</a-button>
        </template>
      </a-table>
    </a-card>

    <AddSampleSqlModal v-model:visible="showAddModal" @success="loadData" />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue';
import { Message } from '@arco-design/web-vue';
import { getDataSourceConfList, getEnabledModelProvider, getSampleSqlList, deleteSampleSql, syncSampleSqlFromLikes, rebuildSampleSqlIndex, getEmbeddingConfig, updateEmbeddingConfig } from '@/api/chatbi';
import AddSampleSqlModal from './components/AddSampleSqlModal.vue';

const columns = [
  { title: 'ID', dataIndex: 'id', ellipsis: true, width: 220 },
  { title: '数据源', dataIndex: 'dataSourceId', width: 150 },
  { title: '问题', dataIndex: 'question', ellipsis: true },
  { title: 'SQL', dataIndex: 'sql', ellipsis: true },
  { title: '创建时间', dataIndex: 'createdAt', width: 180 },
  { title: '操作', slotName: 'optional', width: 80 },
];

const loading = ref(false);
const showAddModal = ref(false);
const tableData = ref([]);
const dataSourceList = ref([]);
const providerList = ref([]);

const form = ref({
  dataSourceId: '',
  providerId: '',
  similarityThreshold: 0.7,
});

const loadData = async () => {
  loading.value = true;
  try {
    const res = await getSampleSqlList({});
    tableData.value = res.data?.data || [];
  } finally {
    loading.value = false;
  }
};

const loadConfig = async () => {
  try {
    const [dsRes, providerRes, configRes] = await Promise.all([
      getDataSourceConfList({}),
      getEnabledModelProvider(),
      getEmbeddingConfig(),
    ]);
    dataSourceList.value = dsRes.data?.data || [];
    providerList.value = providerRes.data?.data || [];
    if (configRes.data) {
      form.value.providerId = configRes.data.providerId;
      form.value.similarityThreshold = configRes.data.similarityThreshold;
    }
  } catch (e) {
    console.error('加载配置失败', e);
  }
};

const saveConfig = async () => {
  try {
    await updateEmbeddingConfig(form.value);
    Message.success('保存成功');
  } catch (e) {
    Message.error('保存失败');
  }
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
</script>

<style scoped>
.sample-sql-management {
  padding: 16px;
}
.action-bar {
  margin-bottom: 16px;
}
</style>
