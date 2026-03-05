<template>
  <a-modal
    :visible="visible"
    :title="currentQuestion?.title || currentQuestion?.question || '请回答'"
    :width="520"
    @cancel="handleCancel"
    @before-ok="handleSubmit"
  >
    <a-tabs v-model:activeTab="activeTab" v-if="questions.length > 1">
      <a-tab-pane
        v-for="(q, index) in questions"
        :key="q.id || index"
        :title="q.title || `问题 ${index + 1}`"
      />
    </a-tabs>

    <a-form :model="form" layout="vertical">
      <!-- 上下文信息 -->
      <a-alert
        v-if="currentQuestion?.context"
        :message="currentQuestion.context"
        type="info"
        class="mb-4"
      />

      <!-- 文本输入 -->
      <a-form-item v-if="currentQuestion?.questionType === 'TEXT'" required>
        <a-input
          v-model="form.answer"
          :placeholder="currentQuestion?.required !== false ? '请输入' : '可选输入'"
        />
      </a-form-item>

      <!-- 单选（支持自定义输入） -->
      <a-form-item v-else-if="currentQuestion?.questionType === 'SELECT'" required>
        <a-select
          v-model="form.answer"
          :placeholder="currentQuestion?.required !== false ? '请选择' : '可选选择'"
          allow-clear
          allow-create
          :style="{ width: '100%' }"
        >
          <a-option v-for="opt in currentQuestion?.options" :key="opt" :value="opt">
            {{ opt }}
          </a-option>
        </a-select>
      </a-form-item>

      <!-- 多选 -->
      <a-form-item v-else-if="currentQuestion?.questionType === 'MULTI_SELECT'" required>
        <a-select
          v-model="form.answer"
          :placeholder="currentQuestion?.required !== false ? '请选择' : '可选选择'"
          multiple
          allow-clear
        >
          <a-option v-for="opt in currentQuestion?.options" :key="opt" :value="opt">
            {{ opt }}
          </a-option>
        </a-select>
      </a-form-item>

      <!-- 日期选择 -->
      <a-form-item v-else-if="currentQuestion?.questionType === 'DATE'" required>
        <a-date-picker
          v-model="form.answer"
          :style="{ width: '100%' }"
          :placeholder="currentQuestion?.required !== false ? '请选择日期' : '可选选择日期'"
        />
      </a-form-item>

      <!-- 数字输入 -->
      <a-form-item v-else-if="currentQuestion?.questionType === 'NUMBER'" required>
        <a-input-number
          v-model="form.answer"
          :min="currentQuestion?.min"
          :max="currentQuestion?.max"
          :placeholder="currentQuestion?.required !== false ? '请输入数字' : '可选输入'"
          :style="{ width: '100%' }"
        />
      </a-form-item>

      <!-- 默认文本输入 -->
      <a-form-item v-else required>
        <a-input
          v-model="form.answer"
          :placeholder="currentQuestion?.required !== false ? '请输入' : '可选输入'"
        />
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<script setup lang="ts">
import { computed, ref, watch } from "vue";

export interface Question {
  id: string;
  question: string;
  questionType?: string;
  options?: string[];
  required?: boolean;
  context?: string;
  title?: string;
  min?: number;
  max?: number;
}

const props = defineProps<{
  visible: boolean;
  questions: Question[];
}>();

const emit = defineEmits<{
  (e: "submit", data: { questionId: string; answer: any }): void;
  (e: "cancel"): void;
}>();

const activeTab = ref(0);
const form = ref<{ answer: any }>({ answer: "" });

const currentQuestion = computed(() => {
  if (!props.questions || props.questions.length === 0) return null;
  return props.questions[activeTab.value] || props.questions[0];
});

watch(
  () => props.visible,
  (val) => {
    if (val) {
      form.value.answer = "";
      activeTab.value = 0;
    }
  }
);

watch(activeTab, () => {
  form.value.answer = "";
});

const handleSubmit = (done: (close: boolean) => void) => {
  const q = currentQuestion.value;
  if (!q) {
    done(true);
    return;
  }

  // 验证必填
  if (q.required !== false && (form.value.answer === "" || form.value.answer === null || form.value.answer === undefined)) {
    done(false);
    return;
  }

  emit("submit", {
    questionId: q.id,
    answer: form.value.answer,
  });
  form.value.answer = "";
  done(true);
};

const handleCancel = () => {
  form.value.answer = "";
  emit("cancel");
};
</script>
