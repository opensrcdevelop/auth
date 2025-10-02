<template>
  <div v-if="message.type === 'TEXT'" v-html="formattedContent"></div>
</template>

<script setup lang="ts">
import {computed} from 'vue';

const props = withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const formattedContent = computed(() => {
  return props.message.content.replace(/\n/g, '<br>');
});
</script>

<style scoped lang="scss"></style>
