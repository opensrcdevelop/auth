<template>
  <div v-if="message.type === 'LOADING'" class="avatar-container">
    <a-avatar class="avatar-assistant">
      <icon-font type="icon-assistant"></icon-font>
    </a-avatar>
    <a-tag :color="message.error ? 'red' : 'arcoblue'">
      <template #icon>
        <icon-close-circle v-if="message.error" />
        <icon-loading v-if="!message.error && message.loading" />
        <icon-check-circle v-if="!message.error && !message.loading" />
      </template>
      {{ message.content }}
    </a-tag>
  </div>
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

<style scoped lang="scss">
.avatar-container {
  display: flex;
  align-items: center;
  gap: 8px;
}

.avatar-assistant {
  background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
  color: "white";
  margin-bottom: 16px;
}
</style>
