<template>
  <router-view />
</template>

<script setup lang="ts">
import { RouterView } from "vue-router";
import { useTaskNotification } from "./hooks/useTaskNotification";

// 全局启用任务通知
useTaskNotification();
</script>
