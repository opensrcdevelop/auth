<template>
  <a-layout class="layout-container">
    <a-layout>
      <Siderbar />
      <a-layout-content class="layout-main" id="main-container">
        <a-spin
          class="spin"
          tip="处理中，请稍后..."
          :loading="globalVariables.apiLoading"
        >
          <router-view v-slot="{ Component, route }">
            <transition name="fade" mode="out-in">
              <component :is="Component" :key="route.path" />
            </transition>
          </router-view>
        </a-spin>
      </a-layout-content>
      <a-back-top
        target-container="#main-container"
        :style="{ position: 'absolute' }"
      >
        <a-button type="primary" shape="circle" size="large">
          <icon-caret-up />
        </a-button>
      </a-back-top>
    </a-layout>
  </a-layout>
</template>

<script setup lang="ts">
import { RouterView } from "vue-router";
import Siderbar from "@/layout/SideBar/index.vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";

const globalVariables = useGlobalVariablesStore();
</script>
