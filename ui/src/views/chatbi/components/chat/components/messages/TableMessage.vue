<template>
  <div v-if="message.type === 'TABLE'" class="table-container">
    <div class="title">
      {{ message.content.title.text }}
    </div>
    <div class="description">
      {{ message.content.title.description }}
    </div>
    <a-table
      column-resizable
      stripe
      :columns="message.content.columns"
      :data="message.content.data"
    />
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
.table-container {
  background-color: #fff;
  border-radius: 8px;
  margin-top: 4px;
  padding: 0 16px 16px 12px;
  width: 100%;

  .title {
    font-size: 16px;
    font-weight: 700;
    color: 1d2129;
    padding: 12px 12px 4px 0;
  }

  .description {
    font-size: 12px;
    color: #86909c;
    font-weight: 400;
    padding: 0 12px 12px 0;
  }
}
</style>
