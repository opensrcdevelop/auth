<template>
  <div class="input-text">
    <div v-if="!inputStatus" class="text-container">
      <div class="text">{{ props.modelValue }}</div>
      <a-tooltip content="编辑">
        <icon-edit class="edit-btn" @click="handleEdit" />
      </a-tooltip>
    </div>
    <div v-else>
      <a-input
        ref="inputRef"
        :default-value="props.modelValue"
        @input="handleInput"
        @blur="handleBlur"
        :placeholder="props.placeholder"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import { watch } from "vue";
import { nextTick, ref } from "vue";

const props = withDefaults(
  defineProps<{
    modelValue: string;
    placeholder?: string;
  }>(),
  {
    modelValue: "",
    placeholder: "请输入",
  }
);

const emits = defineEmits<{
  (e: "update:modelValue", value: string): void;
}>();

const inputRef = ref();
const inputStatus = ref(false);
const inputText = ref(props.modelValue);

const handleEdit = () => {
  inputStatus.value = true;
  nextTick(() => {
    inputRef.value.focus();
  });
};

const handleInput = (val: string) => {
  inputText.value = val;
};

const handleBlur = (ev: FocusEvent) => {
  const val = (ev.target as HTMLInputElement).value;
  emits("update:modelValue", val);
  inputStatus.value = false;
};

/** 输出完成监听 */
let timeout;
watch(inputText, (newVal, oldVal) => {
  clearTimeout(timeout);
  timeout = setTimeout(() => {
    inputStatus.value = false;
    emits("update:modelValue", newVal);
  }, 500);
});
</script>

<style scoped lang="scss">
.input-text {
  width: 100%;
  height: 100%;

  .text-container {
    display: flex;
    justify-content: flex-start;
    align-items: center;

    .text {
      word-break: break-all;
    }

    .edit-btn {
      width: 24px;
      margin-left: 4px;
      cursor: pointer;
      color: rgb(var(--primary-6));
    }
  }
}
</style>
