<template>
  <a-modal
    :visible="visible"
    :mask-closable="false"
    :closable="false"
    :width="1000"
    @close="hadleCloseModal"
    @cancel="hadleCloseModal"
    @ok="handleConfirm"
  >
    <template #title>{{ title }}</template>
    <div class="editor-container">
      <MdEditor
        v-model="text"
        :toolbarsExclude="['image', 'mermaid', 'save', 'catalog', 'github']"
      />
    </div>
  </a-modal>
</template>

<script setup lang="ts">
import {ref, watch} from "vue";
import {MdEditor} from "md-editor-v3";
import "md-editor-v3/lib/style.css";

const props = withDefaults(
  defineProps<{
    content: string;
    visible: boolean;
    title: string;
  }>(),
  {
    content: "",
    visible: false,
    title: "",
  }
);

const emits = defineEmits<{
  (e: "close"): void;
  (e: "confirm", text: string): void;
}>();

const text = ref(props.content);

watch(
  () => props.content,
  (newContent) => {
    text.value = newContent;
  }
);

const hadleCloseModal = () => {
  emits("close");
};

const handleConfirm = () => {
  emits("confirm", text.value);
};
</script>
