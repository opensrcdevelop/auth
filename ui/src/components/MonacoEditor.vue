<template>
  <div ref="monacoEditorRef" :style="monacoEditorStyle"></div>
</template>

<script setup lang="ts">
import { useMonacoEditor } from "@/hooks/useMonacoEditor";
import { computed, onMounted, watch } from "vue";

const props = withDefaults(
  defineProps<{
    width?: string | number;
    height?: string | number;
    language?: string;
    editorOption?: Object;
    modelValue?: string;
  }>(),
  {
    width: "100%",
    height: "100%",
    editorOption: () => ({}),
    modelValue: "",
  }
);

const emits = defineEmits<{
  (e: "blur"): void;
  (e: "update:modelValue", val: string): void;
}>();

const monacoEditorStyle = computed(() => {
  return {
    width: typeof props.width === "string" ? props.width : props.width + "px",
    height: typeof props.height === "string" ? props.height : props.height + "px",
    border: "1px solid var(--color-neutral-2)"
  };
});

const { monacoEditorRef, createEditor, updateVal, updateOptions, getEditor } = useMonacoEditor(props.language);

function updateMonacoVal(val: string) {
  if (val !== getEditor().getValue()) {
    updateVal(val);
  }
}

onMounted(() => {
  const monacoEditor = createEditor();
  updateMonacoVal(props.modelValue);
  updateOptions(props.editorOption);
  monacoEditor?.onDidChangeModelContent(() => {
    emits("update:modelValue", monacoEditor!.getValue());
  });
  monacoEditor?.onDidBlurEditorText(() => {
    emits("blur");
  });
});

watch(
  () => props.modelValue,
  () => {
    updateMonacoVal(props.modelValue);
  }
);

defineExpose({ updateOptions });
</script>
