<template>
  <div class="param-container">
    <div style="width: 100%">
      <a-form
        :model="modelValue"
        :rules="paramConfigRules"
        ref="paramConfigRef"
        layout="vertical"
      >
        <a-row :gutter="24">
          <a-col :span="8">
            <a-form-item field="name" label="参数名称">
              <a-input
                v-model="modelValue.name"
                placeholder="请输入参数名称"
                :error="false"
              />
            </a-form-item>
          </a-col>
          <a-col :span="8">
            <a-form-item field="code" label="参数标识">
              <a-input v-model="modelValue.code" placeholder="请输入参数标识" />
            </a-form-item>
          </a-col>
          <a-col :span="8">
            <a-form-item field="required" label="是否必填">
              <a-switch v-model="modelValue.required" type="round" />
            </a-form-item>
          </a-col>
        </a-row>
        <div v-if="modelValue.type === 'CHOICE'">
          <a-form-item
            v-for="(item, index) in modelValue.options"
            :field="`options[${index}]`"
            label="可选项"
            :hide-label="index !== 0"
            :rules="[{ required: true, message: '选项未填写' }]"
          >
            <a-input
              v-model="modelValue.options[index]"
              placeholder="请输入选项"
            />
            <icon-minus-circle
              v-if="modelValue.options.length > 1"
              @click="handleRemoveOption(index)"
              style="
                margin-left: 4px;
                color: var(--color-neutral-6);
                cursor: pointer;
              "
            />
          </a-form-item>
          <a-button
            style="margin-bottom: 16px"
            size="small"
            shape="circle"
            @click="handleAddOption"
          >
            <icon-plus />
          </a-button>
        </div>
        <a-form-item field="defaultValue" label="默认值">
          <a-input
            v-if="modelValue.type === 'STRING'"
            v-model="props.modelValue.defaultValue"
            placeholder="请输入默认值"
          />
          <a-input-number
            v-if="modelValue.type === 'NUMBER'"
            v-model="props.modelValue.defaultValue"
            placeholder="请输入默认值"
          />
          <a-switch
            v-if="modelValue.type === 'BOOLEAN'"
            v-model="modelValue.defaultValue"
            type="round"
          />
          <a-select
            v-if="modelValue.type === 'CHOICE'"
            allow-clear
            v-model="modelValue.defaultValue"
            placeholder="请选择默认值"
          >
            <a-option
              v-for="(item, index) in modelValue.options.filter((item) => item)"
              :key="index"
              :value="item"
            >
              {{ item }}
            </a-option>
          </a-select>
          <a-textarea
            v-if="modelValue.type === 'LIST'"
            v-model="modelValue.defaultValue"
            placeholder="请输入默认值，使用换行区分"
            :auto-size="{
              minRows: 3,
              maxRows: 8,
            }"
          />
        </a-form-item>
      </a-form>
    </div>
    <div class="right">
      <icon-minus-circle-fill
        @click="handleRemoveParamConfig"
        style="color: var(--color-neutral-6); cursor: pointer"
      />
    </div>
  </div>
</template>

<script setup lang="ts">
import {ref} from "vue";

export interface ParamConfig {
  type: string;
  required: boolean;
  name: string;
  code: string;
  options: string[];
  defaultValue: any;
}

const props = defineProps<{
  modelValue: ParamConfig;
}>();

const emits = defineEmits<{
  (e: "update:modelValue", val: ParamConfig): void;
  (e: "remove"): void;
}>();

const paramConfigRef = ref();
const paramConfigRules = {
  name: [{ required: true, message: "参数名称未填写" }],
  code: [
    { required: true, message: "参数标识未填写" },
    {
      validator: (value, cb) => {
        if (!/^[A-Za-z0-9\_]+$/.test(value.code)) {
          cb("参数标识只允许包含英文字母、数字、下划线_");
        } else {
          cb();
        }
      },
    },
  ],
  required: [{ required: true, message: "是否必填未选择" }],
};

const handleAddOption = () => {
  emits("update:modelValue", {
    ...props.modelValue,
    options: [...props.modelValue.options, ""],
  });
};

const handleRemoveOption = (index: number) => {
  emits("update:modelValue", {
    ...props.modelValue,
    options: props.modelValue.options.filter((_, i) => i !== index),
  });
};

const handleRemoveParamConfig = () => {
  emits("remove");
};

const validate = () => {
  return paramConfigRef.value.validate();
}

const reset = () => {
  paramConfigRef.value.resetFields();
}

defineExpose({
  validate,
  reset
})
</script>

<style lang="scss" scoped>
.param-container {
  border: 1px solid var(--color-neutral-2);
  border-radius: 2px;
  width: 100%;
  padding: 16px;
  display: flex;
  justify-content: space-between;
  align-items: center;

  .right {
    height: 100%;
    margin-left: 24px;
  }
}
</style>
