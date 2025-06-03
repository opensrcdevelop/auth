<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="provider-header">
      <div class="left">
        <div class="title">身份源提供商</div>
        <div class="info">第三方账号提供者。</div>
      </div>
      <a-button type="primary" @click="handleToCreateProvider"
        >创建身份源提供商</a-button
      >
    </div>
    <div class="provider-search">
      <a-input-search
        v-model="searchKeyword"
        @search="handleGetProviderList()"
        @keyup.enter.native="handleGetProviderList()"
        @clear="handleGetProviderList()"
        :style="{ width: '320px' }"
        placeholder="输入身份源提供商名称或标识进行搜索"
        allow-clear
      />
    </div>
    <div class="provider-list" v-if="providerList.length > 0">
      <div
        class="provider-item"
        v-for="item in providerList"
        :key="item.id"
        @click="handleToProviderDetail(item)"
      >
        <div class="logo">
          <img
            style="width: 48px; height: 48px"
            :src="item.logo"
            :draggable="false"
          />
        </div>
        <div class="desc">
          <div class="name">{{ item.name }}</div>
          <div class="info">{{ item.desc }}</div>
        </div>
      </div>
    </div>
    <a-empty style="margin-top: 24px" v-else />
  </div>
</template>
