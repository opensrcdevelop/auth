<template>
  <div class="priority-tag priority-tag-lowest" v-if="priority === -1">
    <span>最低</span>
  </div>
  <div class="priority-tag priority-tag-low" v-if="priority === 0">
    <span>低</span>
  </div>
  <div class="priority-tag priority-tag-medium" v-if="priority === 1">
    <span>中</span>
  </div>
  <div class="priority-tag priority-tag-high" v-if="priority === 2">
    <span>高</span>
  </div>
  <div class="priority-tag priority-tag-highest" v-if="priority === 3">
    <span>最高</span>
  </div>
</template>

<script setup lang="ts">
const props = defineProps({
  priority: {
    type: Number,
    required: true,
  },
});
</script>

<style scoped>
.priority-tag {
  width: 40px;
  height: 22px;
  font-size: 12px;
  font-weight: 700;
  line-height: 22px;
  text-align: center;
  border-radius: 2px;
  color: #fff;
}

.priority-tag-lowest {
  background-color: #0fc6c2;
}

.priority-tag-low {
  background-color: #7bc616;
}

.priority-tag-medium {
  background-color: #ff7d00;
}

.priority-tag-high {
  background-color: #7816ff;
}

.priority-tag-highest {
  background-color: #f53f3f;
}
</style>
