<template>
  <a-modal
    :visible="visible"
    :closable="false"
    :maskClosable="false"
    :title="currentQuestion?.questionText || '请回答 AI 提问'"
    :width="520"
    @cancel="handleCancel"
    @before-ok="handleSubmit"
  >
    <a-radio-group
      type="button"
      v-model="activeQuestion"
      v-if="questions.length > 1"
      style="margin-bottom: 16px"
    >
      <a-radio :value="index" v-for="(q, index) in questions">{{
        `问题 ${index + 1}`
      }}</a-radio>
    </a-radio-group>

    <a-form
      ref="formRef"
      :model="currentForm"
      :rules="formRules"
      layout="vertical"
    >
      <!-- 上下文信息 -->
      <a-alert
        v-if="currentQuestion?.context"
        type="normal"
        :style="{ marginBottom: '16px' }"
        >{{ currentQuestion.context }}</a-alert
      >

      <!-- 文本输入 -->
      <a-form-item
        v-if="currentQuestion?.questionType === 'TEXT'"
        field="answer"
        :required="currentQuestion?.required !== false"
        :label="currentQuestion?.title"
      >
        <a-input
          v-model="currentForm.answer"
          :placeholder="
            currentQuestion?.required !== false ? '请输入' : '可选输入'
          "
        />
      </a-form-item>

      <!-- 单选（支持自定义输入） -->
      <a-form-item
        v-else-if="currentQuestion?.questionType === 'SELECT'"
        field="answer"
        :required="currentQuestion?.required !== false"
        :label="currentQuestion?.title"
      >
        <a-radio-group v-model="currentForm.answer" direction="vertical">
          <a-radio
            v-for="opt in currentQuestion?.options"
            :key="opt"
            :value="opt"
          >
            {{ opt }}
          </a-radio>
          <!-- 其他选项，带输入框 -->
          <a-radio value="__other__">
            <div class="other-radio">
              <span>其他</span>
              <a-input
                v-model="currentForm.customAnswer"
                placeholder="请输入"
                :style="{ width: '160px', marginLeft: '8px' }"
                :disabled="currentForm.answer !== '__other__'"
                @click.stop
              />
            </div>
          </a-radio>
        </a-radio-group>
      </a-form-item>

      <!-- 多选（支持自定义输入） -->
      <a-form-item
        v-else-if="currentQuestion?.questionType === 'MULTI_SELECT'"
        field="answers"
        :required="currentQuestion?.required !== false"
        :label="currentQuestion?.title"
      >
        <a-checkbox-group v-model="currentForm.answers" direction="vertical">
          <a-checkbox
            v-for="opt in currentQuestion?.options"
            :key="opt"
            :value="opt"
          >
            {{ opt }}
          </a-checkbox>
          <!-- 其他选项，带输入框 -->
          <a-checkbox value="__other__">
            <div class="other-checkbox">
              <span>其他</span>
              <a-input
                v-model="currentForm.customAnswer"
                placeholder="请输入"
                :style="{ width: '160px', marginLeft: '8px' }"
                :disabled="!currentForm.answers.includes('__other__')"
                @click.stop
              />
            </div>
          </a-checkbox>
        </a-checkbox-group>
      </a-form-item>

      <!-- 默认文本输入 -->
      <a-form-item
        v-else
        field="answer"
        :required="currentQuestion?.required !== false"
        :label="currentQuestion?.title"
      >
        <a-input
          v-model="currentForm.answer"
          :placeholder="
            currentQuestion?.required !== false ? '请输入' : '可选输入'
          "
        />
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<script setup lang="ts">
import {computed, nextTick, onUnmounted, ref, watch} from "vue";
import {Message} from "@arco-design/web-vue";

export interface Question {
  id: string;
  questionText: string;
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

const formRef = ref<any>(null);

let timeoutTimer: ReturnType<typeof setTimeout> | null = null;
const TIMEOUT_MS = 60 * 1000 * 2; // 2 分钟

const activeQuestion = ref(0);
// 存储所有问题的回答，key 是问题 id
const forms = ref<
  Record<string, { answer: string; answers: string[]; customAnswer: string }>
>({});

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
  return props.questions[activeQuestion.value] || props.questions[0];
});

const formRules = computed(() => {
  const q = currentQuestion.value;
  if (!q || q.required === false) return {};

  const rules: Record<string, any> = {};
  if (q.questionType === "MULTI_SELECT") {
    rules.answers = [
      {
        type: "array",
        required: true,
        message: `请选择：${q.title || q.questionText}`,
      },
    ];
  } else if (q.questionType === "SELECT") {
    rules.answer = [
      {
        type: "string",
        required: true,
        message: `请选择：${q.title || q.questionText}`,
      },
    ];
  } else {
    rules.answer = [
      {
        type: "string",
        required: true,
        message: `请输入`,
      },
    ];
  }
  return rules;
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
      activeQuestion.value = 0;

      // 启动 2 分钟超时定时器
      timeoutTimer = setTimeout(() => {
        handleCancel();
      }, TIMEOUT_MS);
    } else {
      // 关闭对话框时清除定时器
      if (timeoutTimer) {
        clearTimeout(timeoutTimer);
        timeoutTimer = null;
      }
    }
  },
);

const handleSubmit = async (done: (close: boolean) => void) => {
  await nextTick();

  const validateRes = await formRef.value?.validate();
  if (validateRes) {
    done(false);
    return;
  }

  // 自定义验证：选择了"其他"但没有输入内容
  const currentQ = currentQuestion.value;
  if (currentQ) {
    const form = currentForm.value;
    if (
      currentQ.questionType === "SELECT" &&
      form.answer === "__other__" &&
      !form.customAnswer
    ) {
      Message.warning("请输入其他内容");
      done(false);
      return;
    }
    if (
      currentQ.questionType === "MULTI_SELECT" &&
      form.answers?.includes("__other__") &&
      !form.customAnswer
    ) {
      Message.warning("请输入其他内容");
      done(false);
      return;
    }
  }

  // 验证所有必填问题
  for (const q of props.questions) {
    if (q.required === false) continue;

    const form = forms.value[q.id] || {
      answer: "",
      answers: [],
      customAnswer: "",
    };
    let isValid = false;

    if (q.questionType === "MULTI_SELECT") {
      isValid = form.answers && form.answers.length > 0;
      // 如果选择了"其他"选项，必须填写自定义内容
      if (isValid && form.answers.includes("__other__")) {
        isValid = !!form.customAnswer && form.customAnswer.trim() !== "";
      }
    } else if (q.questionType === "SELECT") {
      if (form.answer === "__other__") {
        // 选择了"其他"选项，必须填写自定义内容
        isValid = !!form.customAnswer && form.customAnswer.trim() !== "";
      } else {
        isValid = form.answer && form.answer !== "";
      }
    } else {
      isValid = form.answer && form.answer !== "";
    }

    if (!isValid) {
      // 切换到未填写的问题
      const index = props.questions.findIndex((item) => item.id === q.id);
      if (index !== -1) {
        activeQuestion.value = index;
      }
      done(false);
      return;
    }
  }

  // 收集所有问题的回答
  const allAnswers: { questionId: string; answer: any }[] = [];

  for (const q of props.questions) {
    const form = forms.value[q.id] || {
      answer: "",
      answers: [],
      customAnswer: "",
    };

    let answer: any;
    if (q.questionType === "MULTI_SELECT") {
      const answerList = [...form.answers];
      // 如果选择了"其他"选项，将自定义输入的值添加到答案中
      const otherIndex = answerList.indexOf("__other__");
      if (otherIndex !== -1 && form.customAnswer) {
        answerList.splice(otherIndex, 1, form.customAnswer);
      }
      // 转为逗号分隔的字符串
      answer = answerList.join(",");
    } else {
      answer = form.answer;
      // 如果选择了"其他"选项，使用自定义输入的值
      if (answer === "__other__" && form.customAnswer) {
        answer = form.customAnswer;
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
  if (timeoutTimer) {
    clearTimeout(timeoutTimer);
    timeoutTimer = null;
  }
  forms.value = {};
  emit("cancel");
};

onUnmounted(() => {
  if (timeoutTimer) {
    clearTimeout(timeoutTimer);
    timeoutTimer = null;
  }
});
</script>

<style scoped>
.other-radio,
.other-checkbox {
  display: inline-flex;
  align-items: center;
}
</style>
