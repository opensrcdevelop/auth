<template>
  <div v-if="message.type === 'TEXT'" v-text="message.content"></div>
</template>

<script setup lang="ts">
withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);
</script>

<style scoped lang="scss"></style>
