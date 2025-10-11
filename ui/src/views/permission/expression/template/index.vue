<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <div class="template-header">
      <div class="left">
        <div class="title">限制条件模版</div>
        <div class="info">支持创建模版，复用限制条件。</div>
      </div>
      <a-button type="primary" @click="handleToCreateTemplate"
        >创建限制条件模版</a-button
      >
    </div>
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入模板名称进行搜索"
      allow-clear
      v-model="searchKeyword"
      @search="handleGetTemplateList()"
      @keyup.enter.native="handleGetTemplateList()"
      @clear="handleGetTemplateList()"
    />
    <div class="template-list" v-if="templateList.length > 0">
      <div
        class="template-item"
        v-for="item in templateList"
        :key="item.id"
        @click="handleToTemplateDetail(item.id)"
      >
        <div class="icon">
          <svg
            t="1751901319177"
            class="icon"
            viewBox="0 0 1024 1024"
            version="1.1"
            xmlns="http://www.w3.org/2000/svg"
            p-id="10006"
            width="64"
            height="64"
          >
            <path
              d="M358.4 298.666667a85.333333 85.333333 0 0 1 85.333333-85.333334h85.333334a42.666667 42.666667 0 0 0 42.666666-42.666666 42.666667 42.666667 0 0 0-42.666666-42.666667h-85.333334a170.666667 170.666667 0 0 0-170.666666 170.666667v597.333333a42.666667 42.666667 0 0 0 42.666666 42.666667 42.666667 42.666667 0 0 0 42.666667-42.666667z"
              fill="#fff"
              p-id="10007"
            ></path>
            <path
              d="M145.066667 426.666667m42.666666 0l298.666667 0q42.666667 0 42.666667 42.666666l0 0q0 42.666667-42.666667 42.666667l-298.666667 0q-42.666667 0-42.666666-42.666667l0 0q0-42.666667 42.666666-42.666666Z"
              fill="#fff"
              p-id="10008"
            ></path>
            <path
              d="M865.706667 426.666667a42.666667 42.666667 0 0 0-37.12 21.333333l-85.333334 149.333333-85.333333-149.333333a42.666667 42.666667 0 0 0-37.12-21.333333 42.666667 42.666667 0 0 0-36.693333 64l110.933333 192-110.933333 192a42.666667 42.666667 0 0 0 36.693333 64 42.666667 42.666667 0 0 0 37.12-21.333334l85.333333-149.333333 85.333334 149.333333a42.666667 42.666667 0 0 0 37.12 21.333334 42.666667 42.666667 0 0 0 36.693333-64l-110.933333-192 110.933333-192a42.666667 42.666667 0 0 0-36.693333-64z"
              fill="#fff"
              p-id="10009"
            ></path>
          </svg>
        </div>
        <div class="info">
          <div class="name">{{ item.name }}</div>
          <div class="desc">{{ item.desc }}</div>
        </div>
        <div class="action">
          <icon-delete
            style="color: rgb(var(--red-6))"
            @click.stop="handleDeleteTemplate(item)"
          />
        </div>
      </div>
    </div>
    <a-empty style="margin-top: 24px" v-else />
  </div>
</template>
