<template>
  <div class="page-container">
    <div class="back" @click="handleBack">
      <icon-left class="icon" />
      <div class="text">返回</div>
    </div>
    <div class="content">
      <slot></slot>
    </div>
  </div>
</template>

<script setup lang="ts">
const emits = defineEmits<{
  (e: "back"): void;
}>();

const handleBack = () => {
  emits("back");
};
</script>

<style scoped lang="scss">
.page-container {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;

  .back {
    display: flex;
    align-items: center;
    margin-bottom: 16px;
    color: #8a92a6;
    width: 64px;
    cursor: pointer;
    transition: all 0.3s linear;
    user-select: none;

    .text {
      margin-left: 7px;
    }
  }

  .back:hover {
    color: #215ae5;
  }
}
</style>
