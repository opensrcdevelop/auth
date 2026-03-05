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
        <a-radio-group v-model="form.answer">
          <a-radio v-for="opt in currentQuestion?.options" :key="opt" :value="opt">
            {{ opt }}
          </a-radio>
        </a-radio-group>
        <div class="mt-3">
          <a-input
            v-model="form.customAnswer"
            placeholder="或输入自定义内容"
            @press-enter="handleCustomAnswerSelect"
          >
            <template #append>
              <a-button @click="handleCustomAnswerSelect">添加</a-button>
            </template>
          </a-input>
        </div>
      </a-form-item>

      <!-- 多选（支持自定义输入） -->
      <a-form-item v-else-if="currentQuestion?.questionType === 'MULTI_SELECT'" required>
        <a-checkbox-group v-model="form.answers">
          <a-checkbox v-for="opt in currentQuestion?.options" :key="opt" :value="opt">
            {{ opt }}
          </a-checkbox>
        </a-checkbox-group>
        <div class="mt-3">
          <a-input
            v-model="form.customAnswer"
            placeholder="或输入自定义内容"
            @press-enter="handleCustomMultiAnswer"
          >
            <template #append>
              <a-button @click="handleCustomMultiAnswer">添加</a-button>
            </template>
          </a-input>
        </div>
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
const form = ref<{ answer: string; answers: string[]; customAnswer: string }>({
  answer: "",
  answers: [],
  customAnswer: "",
});

const currentQuestion = computed(() => {
  if (!props.questions || props.questions.length === 0) return null;
  return props.questions[activeTab.value] || props.questions[0];
});

watch(
  () => props.visible,
  (val) => {
    if (val) {
      form.value.answer = "";
      form.value.answers = [];
      form.value.customAnswer = "";
      activeTab.value = 0;
    }
  }
);

watch(activeTab, () => {
  form.value.answer = "";
  form.value.answers = [];
  form.value.customAnswer = "";
});

// 单选自定义输入
const handleCustomAnswerSelect = () => {
  if (form.value.customAnswer && form.value.customAnswer.trim()) {
    form.value.answer = form.value.customAnswer.trim();
    form.value.customAnswer = "";
  }
};

// 多选自定义输入
const handleCustomMultiAnswer = () => {
  if (form.value.customAnswer && form.value.customAnswer.trim()) {
    if (!form.value.answers.includes(form.value.customAnswer.trim())) {
      form.value.answers.push(form.value.customAnswer.trim());
    }
    form.value.customAnswer = "";
  }
};

const handleSubmit = (done: (close: boolean) => void) => {
  const q = currentQuestion.value;
  if (!q) {
    done(true);
    return;
  }

  let answer: any;
  if (q.questionType === "MULTI_SELECT") {
    answer = form.value.answers;
  } else {
    answer = form.value.answer;
  }

  // 验证必填
  if (q.required !== false) {
    if (q.questionType === "MULTI_SELECT") {
      if (!answer || answer.length === 0) {
        done(false);
        return;
      }
    } else {
      if (!answer || answer === "") {
        done(false);
        return;
      }
    }
  }

  emit("submit", {
    questionId: q.id,
    answer: answer,
  });
  form.value.answer = "";
  form.value.answers = [];
  form.value.customAnswer = "";
  done(true);
};

const handleCancel = () => {
  form.value.answer = "";
  form.value.answers = [];
  form.value.customAnswer = "";
  emit("cancel");
};
</script>
