<template>
  <a-modal v-model:visible="visible" title="添加示例 SQL" @ok="handleSubmit" @cancel="visible = false">
    <a-form :model="form" layout="vertical">
      <a-form-item label="数据源" required>
        <a-select v-model="form.dataSourceId" placeholder="请选择数据源">
          <a-option v-for="ds in dataSourceList" :key="ds.id" :value="ds.id">
            {{ ds.name }}
          </a-option>
        </a-select>
      </a-form-item>
      <a-form-item label="问题" required>
        <a-textarea v-model="form.question" placeholder="请输入问题" :rows="3" />
      </a-form-item>
      <a-form-item label="SQL" required>
        <a-textarea v-model="form.sql" placeholder="请输入 SQL" :rows="5" />
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<script setup lang="ts">
import {ref, watch} from 'vue';
import {Message} from '@arco-design/web-vue';
import {addSampleSql} from '@/api/chatbi';

const props = defineProps<{
  visible: boolean;
  dataSourceList: any[];
}>();

const emit = defineEmits<{
  (e: 'update:visible', value: boolean): void;
  (e: 'success'): void;
}>();

const visible = ref(false);

const form = ref({
  dataSourceId: '',
  question: '',
  sql: '',
});

watch(() => props.visible, (val) => {
  visible.value = val;
});

watch(visible, (val) => {
  emit('update:visible', val);
});

const handleSubmit = async () => {
  if (!form.value.dataSourceId || !form.value.question || !form.value.sql) {
    Message.warning('请填写完整信息');
    return;
  }

  try {
    await addSampleSql(form.value);
    Message.success('添加成功');
    visible.value = false;
    form.value = { dataSourceId: '', question: '', sql: '' };
    emit('success');
  } catch (e) {
    Message.error('添加失败');
  }
};
</script>
