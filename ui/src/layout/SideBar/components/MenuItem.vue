<template>
  <a-sub-menu v-if="menu.children?.length > 0" :key="menu.path">
    <template #icon>
      <icon-font :type="menu.meta.icon" style="font-size: 16px;"></icon-font>
    </template>
    <template #title>{{ menu.meta.title }}</template>
    <menu-item
      v-for="sub in menu.children"
      :key="sub.path"
      :menu="sub"
    ></menu-item>
  </a-sub-menu>
  <a-menu-item v-if="!menu.children" :key="menu.path" @click="$router.push(menu)">
    <template #icon>
      <icon-font :type="menu.meta.icon" style="font-size: 16px;"></icon-font>
    </template>
    {{ menu.meta.title }}
  </a-menu-item>
</template>

<script setup lang="ts">
import type { PropType } from "vue";

defineProps({
  menu: {
    type: Object as PropType<any>,
    required: true,
  },
});
</script>
