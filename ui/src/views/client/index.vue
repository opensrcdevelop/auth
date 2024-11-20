<script lang="ts">
import clientTs from "./index";
export default clientTs;
</script>

<style lang="scss" scoped>
@import "./index.scss";
</style>

<template>
  <div>
    <div class="client-header">
      <div class="left">
        <div class="title">客户端</div>
        <div class="info">创建客户端进行身份验证。</div>
      </div>
      <a-button type="primary" @click="toCreateClient">创建客户端</a-button>
    </div>

    <a-tabs default-active-key="1">
      <a-tab-pane key="1" title="客户端列表">
        <div class="client-search">
          <a-input-search
            v-model="searchKeyword"
            @search="handleGetClientList(searchKeyword)"
            @keyup.enter.native="handleGetClientList(searchKeyword)"
            :style="{ width: '320px' }"
            placeholder="输入客户端名称进行搜索"
            allow-clear
          />
        </div>
        <div class="client-list" v-if="clientList!.length > 0">
          <div
            class="client-card"
            v-for="(client, index) in clientList"
            :key="client.id"
          >
            <div class="client-info">
              <div @click="toClientDetail(client.id)">
                <div class="name">{{ client.name }}</div>
                <div class="info">{{ client.desc }}</div>
              </div>

              <div class="operation">
                <a-dropdown trigger="hover">
                  <a-button type="text">
                    <template #icon>
                      <icon-more />
                    </template>
                  </a-button>
                  <template #content>
                    <a-doption>
                      <div
                        style="font-size: 12px; color: #e8353e; height: 24px; line-height: 24px;"
                        @click="handleDeleteClient(client)"
                      >
                        <icon-delete /> 删除
                      </div>
                    </a-doption>
                  </template>
                </a-dropdown>
              </div>
            </div>
            <div class="client-id">Client ID: {{ client.id }}</div>
          </div>
        </div>
        <a-empty v-else />
      </a-tab-pane>
    </a-tabs>
  </div>
</template>
