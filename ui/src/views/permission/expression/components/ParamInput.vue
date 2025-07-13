<template>
  <div class="param-container">
    <a-form :model="paramsForm" ref="paramsFormRef" layout="vertical">
      <a-form-item
        v-for="(config, index) in configs"
        :key="index"
        :label="config.name"
        :field="`params[${index}].value`"
        :rules="[
          { required: config.required, message: `${config.name}是必填项` },
        ]"
      >
        <a-input
          v-if="config.type === 'STRING'"
          v-model="modelValue[index].value"
          :placeholder="`请输入${config.name}`"
        />
        <a-input-number
          v-if="config.type === 'NUMBER'"
          v-model="modelValue[index].value"
          :placeholder="`请输入${config.name}`"
        />
        <a-switch
          v-if="config.type === 'BOOLEAN'"
          v-model="modelValue[index].value"
          type="round"
        />
        <a-select
          v-if="config.type === 'CHOICE'"
          allow-clear
          v-model="modelValue[index].value"
          :placeholder="`请选择${config.name}`"
        >
          <a-option
            v-for="(option, index) in config.options"
            :key="index"
            :value="option"
          >
            {{ option }}
          </a-option>
        </a-select>
        <a-textarea
          v-if="config.type === 'LIST'"
          v-model="modelValue[index].value"
          :placeholder="`请输入${config.name}，使用换行区分`"
          :auto-size="{
            minRows: 3,
            maxRows: 8,
          }"
        />
      </a-form-item>
    </a-form>
  </div>
</template>

<script setup lang="ts">
import {reactive, ref} from "vue";
import {ParamConfig} from "../template/components/ParamSelect.vue";

const props = defineProps<{
  modelValue: [];
  configs: ParamConfig;
}>();

const paramsForm = reactive({
  params: props.modelValue,
})

const paramsFormRef = ref(null);

const validate = () => {
  return paramsFormRef.value.validate();
}

const reset = () => {
  paramsFormRef.value.resetFields();
}

defineExpose({
  validate,
  reset
})
</script>

<style lang="scss" scoped></style>
