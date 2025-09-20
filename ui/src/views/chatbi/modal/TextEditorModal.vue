<template>
  <a-modal
    :visible="visible"
    :mask-closable="false"
    :closable="false"
    :width="800"
    @close="hadleCloseModal"
    @cancel="hadleCloseModal"
    @ok="handleConfirm"
  >
    <template #title>{{ title }}</template>
    <div class="editor-container">
      <a-textarea
        v-model="text"
        :auto-size="{
          minRows: 3,
          maxRows: 8,
        }"
      />
    </div>
  </a-modal>
</template>

<script setup lang="ts">
import {ref, watch} from "vue";

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
  text.value = "";
  emits("close");
};

const handleConfirm = () => {
  emits("confirm", text.value);
  text.value = "";
};
</script>
