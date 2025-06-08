<template>
  <div class="input-container">
    <a-input-password
      v-if="type === 'password'"
      v-model="password"
      :placeholder="props.placeholder"
      :error="isError"
      @input="doCheck"
    >
      <template #suffix>
        <div class="icon" v-if="loading" style="margin-left: 4px;">
          <icon-loading />
        </div>
        <div class="icon" v-else style="margin-left: 4px;">
          <icon-check-circle
            v-if="!isError && password"
            style="color: rgb(var(--green-6))"
          />
          <icon-close-circle
            v-if="isError && password"
            style="color: rgb(var(--red-6))"
          />
        </div>
      </template>
    </a-input-password>
    <a-input
      v-if="type === 'text'"
      v-model="password"
      :placeholder="props.placeholder"
      :error="isError"
      @input="doCheck"
    >
      <template #suffix>
        <div class="icon" v-if="loading">
          <icon-loading />
        </div>
        <div class="icon" v-else>
          <icon-check-circle
            v-if="!isError && password"
            style="color: rgb(var(--green-6))"
          />
          <icon-close-circle
            v-if="isError && password"
            style="color: rgb(var(--red-6))"
          />
        </div>
      </template>
    </a-input>
    <div class="error-text" v-if="isError && !hasRuleResults">
      {{ errorText }}
    </div>
    <transition name="slide-down">
      <div class="rule-result" v-if="isError && hasRuleResults">
        <div class="rule-text">你的密码需要满足如下要求</div>
        <div v-for="rule in props.checkRes?.ruleResults" :key="rule.rule">
          <div class="rule-text" v-if="rule.valid">
            <icon-check-circle style="color: rgb(var(--green-6))" />
            {{ rule.rule }}
          </div>
          <div class="rule-text" v-else>
            <icon-close-circle style="color: rgb(var(--red-6))" />
            {{ rule.rule }}
          </div>
        </div>
      </div>
    </transition>
  </div>
</template>

<script setup lang="ts">
import {computed, ref} from "vue";

const props = defineProps({
  type: {
    type: String,
    required: true,
  },
  loading: {
    type: Boolean,
    required: false,
  },
  checkRes: {
    type: Object as () => CheckResult,
    required: false,
  },
  placeholder: {
    type: String,
    required: false,
  },
});

interface RuleResult {
  rule: string;
  valid: boolean;
}

interface CheckResult {
  errorMessage: string;
  valid: boolean;
  ruleResults: RuleResult[];
}

const emits = defineEmits<{
  (e: "check", value: string): void;
}>();

const password = ref("");

const isError = computed(() => {
  if (password.value.length === 0) {
    return false;
  }
  return props.checkRes?.valid === false;
});

const errorText = computed(() => {
  if (props.checkRes?.errorMessage) {
    return props.checkRes.errorMessage;
  }
  return "";
});

const hasRuleResults = computed(() => {
  if (props.checkRes?.ruleResults) {
    return props.checkRes.ruleResults.length > 0;
  }
  return false;
});

const doCheck = () => {
  if (password.value) {
    emits("check", password.value);
  }
};

const setPassword = (value: string) => {
  password.value = value;
  doCheck();
}

defineExpose({
  setPassword,
})
</script>

<style scoped lang="scss">
.input-container {
  width: 100%;

  .icon {
    color: rgb(var(--arcoblue-6));
  }
}

.error-text {
  color: rgb(var(--red-6));
  font-size: 12px;
  margin-top: 4px;
}

.rule-result {
  margin-top: 12px;
  color: var(--color-text-2);
  padding: 12px;
  border-radius: 4px;
  border: 1px solid #eeeff1;
  transition: all 0.3s;

  .rule-text {
    line-height: 24px;
  }
}

.slide-down-enter-active,
.slide-down-leave-active {
  transition: all 0.3s ease;
  overflow: hidden;
}

.slide-down-enter-from,
.slide-down-leave-to {
  opacity: 0;
  transform: translateY(-8px);
}
</style>
