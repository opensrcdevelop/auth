<template>
  <div v-if="message.type === 'CHART'" class="echarts-container">
    <div class="title">
      {{ message.content.title.text }}
    </div>
    <div class="description">
      {{ message.content.title.description }}
    </div>
    <div
      class="chart"
      :ref="(el) => initChart(el, message.content?.option)"
    ></div>
  </div>
</template>

<script setup lang="ts">
import * as echarts from "echarts";
import {onUnmounted} from "vue";

withDefaults(
  defineProps<{
    message: any;
  }>(),
  {
    message: {},
  }
);

const chartRefs = new Map<any, any>();

const initChart = (el, option) => {
  if (!el || !option) return;

  const existingChart = chartRefs.get(el);
  if (existingChart) {
    const existingOption = existingChart.getOption();
    const isOptionChanged =
      JSON.stringify(existingOption) !== JSON.stringify(option);

    if (!isOptionChanged) {
      return;
    }

    existingChart.setOption(option, true);
    return;
  }

  setTimeout(() => {
    const chart = echarts.init(el);
    chart.setOption(option);

    chart.resize();

    const resizeObserver = new ResizeObserver(() => {
      chart.resize();
    });

    resizeObserver.observe(el);
    chartRefs.set(el, chart);
    chartRefs.set(el + "_observer", resizeObserver);
  }, 0);
};

onUnmounted(() => {
  chartRefs.forEach((value, key) => {
    if (typeof key === "string" && key.endsWith("_observer")) {
      value.disconnect();
    } else {
      value.dispose();
    }
  });
  chartRefs.clear();
});
</script>

<style scoped lang="scss">
.echarts-container {
  background-color: #fff;
  border-radius: 8px;
  margin-top: 4px;
  width: 100%;

  .title {
    font-size: 16px;
    font-weight: 700;
    color: 1d2129;
    padding: 12px 12px 4px 12px;
  }

  .description {
    font-size: 12px;
    color: #86909c;
    font-weight: 400;
    padding: 0 12px;
  }

  .chart {
    height: 330px;
    width: 100%;
  }
}
</style>
