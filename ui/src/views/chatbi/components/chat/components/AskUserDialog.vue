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

    <a-form :model="currentForm" layout="vertical">
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
          v-model="currentForm.answer"
          :placeholder="currentQuestion?.required !== false ? '请输入' : '可选输入'"
        />
      </a-form-item>

      <!-- 单选（支持自定义输入） -->
      <a-form-item v-else-if="currentQuestion?.questionType === 'SELECT'" required>
        <a-radio-group v-model="currentForm.answer">
          <a-radio v-for="opt in currentQuestion?.options" :key="opt" :value="opt">
            {{ opt }}
          </a-radio>
        </a-radio-group>
        <div class="mt-3">
          <a-input
            v-model="currentForm.customAnswer"
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
        <a-checkbox-group v-model="currentForm.answers">
          <a-checkbox v-for="opt in currentQuestion?.options" :key="opt" :value="opt">
            {{ opt }}
          </a-checkbox>
        </a-checkbox-group>
        <div class="mt-3">
          <a-input
            v-model="currentForm.customAnswer"
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
          v-model="currentForm.answer"
          :placeholder="currentQuestion?.required !== false ? '请输入' : '可选输入'"
        />
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<script setup lang="ts">
import { computed, ref, watch } from "vue";
import { Message } from "@arco-design/web-vue";

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
  (e: "submit", data: { answers: { questionId: string; answer: any }[] }): void;
  (e: "cancel"): void;
}>();

const activeTab = ref(0);
// 存储所有问题的回答，key 是问题 id
const forms = ref<Record<string, { answer: string; answers: string[]; customAnswer: string }>>({});

const currentForm = computed(() => {
  const q = currentQuestion.value;
  if (!q) return { answer: "", answers: [], customAnswer: "" };
  if (!forms.value[q.id]) {
    forms.value[q.id] = { answer: "", answers: [], customAnswer: "" };
  }
  return forms.value[q.id];
});

const currentQuestion = computed(() => {
  if (!props.questions || props.questions.length === 0) return null;
  return props.questions[activeTab.value] || props.questions[0];
});

watch(
  () => props.visible,
  (val) => {
    if (val) {
      // 初始化所有问题的表单
      forms.value = {};
      props.questions.forEach((q) => {
        forms.value[q.id] = { answer: "", answers: [], customAnswer: "" };
      });
      activeTab.value = 0;
    }
  }
);

watch(activeTab, () => {
  // 切换标签时保存当前表单状态（通过 computed 已自动处理）
});

// 单选自定义输入
const handleCustomAnswerSelect = () => {
  const form = currentForm.value;
  if (form.customAnswer && form.customAnswer.trim()) {
    form.answer = form.customAnswer.trim();
    form.customAnswer = "";
  }
};

// 多选自定义输入
const handleCustomMultiAnswer = () => {
  const form = currentForm.value;
  if (form.customAnswer && form.customAnswer.trim()) {
    if (!form.answers.includes(form.customAnswer.trim())) {
      form.answers.push(form.customAnswer.trim());
    }
    form.customAnswer = "";
  }
};

const handleSubmit = (done: (close: boolean) => void) => {
  // 收集所有问题的回答
  const allAnswers: { questionId: string; answer: any }[] = [];

  for (const q of props.questions) {
    const form = forms.value[q.id] || { answer: "", answers: [], customAnswer: "" };

    let answer: any;
    if (q.questionType === "MULTI_SELECT") {
      answer = form.answers;
    } else {
      answer = form.answer;
    }

    // 验证必填
    if (q.required !== false) {
      if (q.questionType === "MULTI_SELECT") {
        if (!answer || answer.length === 0) {
          Message.warning(`请回答问题：${q.title || q.question}`);
          done(false);
          return;
        }
      } else {
        if (!answer || answer === "") {
          Message.warning(`请回答问题：${q.title || q.question}`);
          done(false);
          return;
        }
      }
    }

    allAnswers.push({
      questionId: q.id,
      answer: answer,
    });
  }

  emit("submit", {
    answers: allAnswers,
  });
  // 重置表单
  forms.value = {};
  done(true);
};

const handleCancel = () => {
  form.value.answer = "";
  form.value.answers = [];
  form.value.customAnswer = "";
  emit("cancel");
};
</script>
