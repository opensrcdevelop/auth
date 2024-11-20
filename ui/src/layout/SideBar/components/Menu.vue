<template>
  <a-scrollbar style="overflow: auto;">
    <a-menu theme="light" :default-selected-keys="defaultSelected">
      <menu-item
        v-for="menu in menuRoutes"
        :key="menu.path"
        :menu="menu"
      ></menu-item>
    </a-menu>
  </a-scrollbar>
</template>

<script setup lang="ts">
import MenuItem from "./MenuItem.vue";
import { useRoute } from "vue-router";
import { menuRoutes } from "@/router";
import { computed } from "vue";

const route = useRoute();

const defaultSelected = computed(() => {
  if (route.meta.parent) {
    return [route.meta.parent]
  } else {
    return [route.path]
  }
});
</script>
