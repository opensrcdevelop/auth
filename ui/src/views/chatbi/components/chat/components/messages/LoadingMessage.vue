<template>
  <div v-if="message.type === 'LOADING' || message.type === 'ERROR'" class="avatar-container">
    <a-avatar class="avatar-assistant">
      <icon-font type="icon-assistant" />
    </a-avatar>
    <a-tag :color="message.type === 'ERROR' ? 'red' : 'arcoblue'">
      <template #icon>
        <icon-close-circle v-if="message.type === 'ERROR'" />
        <icon-loading v-else-if="message.loading" />
        <icon-check-circle v-else />
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
