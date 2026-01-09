<template>
  <div v-if="columnsLoaded">
    <div class="conditions-container">
      <div
        class="conjunction-container"
        v-if="conditions.filters?.length + conditions.groups?.length > 1"
      >
        <div class="conjunction-line-top"></div>
        <div class="conjunction" @click="handleSwapperConjunction">
          <a-space>
            <span v-if="conditions.conjunction === 'AND'">且</span>
            <span v-if="conditions.conjunction === 'OR'">或</span>
            <icon-swap />
          </a-space>
        </div>
        <div class="conjunction-line-btm"></div>
      </div>
      <div class="conditions-content-container">
        <div>
          <a-form :model="props.conditions" ref="formRef">
            <a-row
              :gutter="8"
              v-for="(filter, index) in props.conditions?.filters"
              :key="index"
            >
              <a-col :span="8">
                <a-form-item
                  :field="`filters[${index}].key`"
                  hide-label
                  :rules="[{ required: true, message: '字段未选择' }]"
                >
                  <a-select
                    placeholder="请选择字段"
                    v-model:model-value="filter.key"
                    @change="handleUserColumnsSelectChange"
                    allow-search
                  >
                    <a-option
                      v-for="column in allUserColumns"
                      :key="column.key"
                      :value="column.key"
                      :disabled="
                        conditions.filters.some(
                          (item) => item.key === column.key
                        )
                      "
                    >
                      {{ column.name }}</a-option
                    >
                  </a-select>
                </a-form-item>
              </a-col>
              <a-col :span="6">
                <a-form-item
                  :field="`filters[${index}].filterType`"
                  hide-label
                  :rules="[{ required: true, message: '运算符未选择' }]"
                >
                  <a-select
                    placeholder="请选择运算符"
                    v-model:model-value="filter.filterType"
                  >
                    <a-option
                      value="EQ"
                      v-if="
                        [
                          'STRING',
                          'NUMBER',
                          'BOOLEAN',
                          'DATETIME',
                          'DATE',
                          'DICT',
                        ].includes(filter.dataType)
                      "
                      >等于</a-option
                    >
                    <a-option
                      value="NE"
                      v-if="
                        [
                          'STRING',
                          'NUMBER',
                          'BOOLEAN',
                          'DATETIME',
                          'DATE',
                          'DICT',
                        ].includes(filter.dataType)
                      "
                      >不等于</a-option
                    >
                    <a-option
                      value="LIKE"
                      v-if="['STRING'].includes(filter.dataType)"
                      >包含</a-option
                    >
                    <a-option
                      value="NOT_LIKE"
                      v-if="['STRING'].includes(filter.dataType)"
                      >不包含</a-option
                    >
                    <a-option
                      value="GT"
                      v-if="
                        ['NUMBER', 'DATETIME', 'DATE'].includes(filter.dataType)
                      "
                      >大于</a-option
                    >
                    <a-option
                      value="LT"
                      v-if="
                        ['NUMBER', 'DATETIME', 'DATE'].includes(filter.dataType)
                      "
                      >小于</a-option
                    >
                  </a-select>
                </a-form-item>
              </a-col>
              <a-col :span="10">
                <a-form-item
                  :field="`filters[${index}].value`"
                  hide-label
                  :rules="[{ required: true, message: '未输入 / 选择' }]"
                >
                  <a-input-number
                    v-if="filter.dataType === 'NUMBER'"
                    hide-button
                    v-model="filter.value"
                    placeholder="请输入"
                  />
                  <a-input
                    v-if="filter.dataType === 'STRING'"
                    v-model="filter.value"
                    placeholder="请输入"
                  />
                  <a-select
                    v-if="filter.dataType === 'BOOLEAN'"
                    v-model="filter.value"
                    placeholder="请选择"
                  >
                    <a-option value="true">是</a-option>
                    <a-option value="false">否</a-option>
                  </a-select>
                  <a-date-picker
                    style="width: 100%"
                    v-if="filter.dataType === 'DATETIME'"
                    show-time
                    value-format="timestamp"
                    v-model="filter.value"
                  />
                  <a-date-picker
                    style="width: 100%"
                    v-if="filter.dataType === 'DATE'"
                    value-format="timestamp"
                    v-model="filter.value"
                  />
                  <a-select
                    v-if="filter.dataType === 'DICT' && !filter.cascadeDict"
                    v-model="filter.value"
                    placeholder="请选择"
                  >
                    <a-option
                      :value="dictData.id"
                      v-for="dictData in allDictDatas[filter.key]"
                      :key="dictData.id"
                      >{{ dictData.label }}</a-option
                    >
                  </a-select>
                  <a-cascader
                    v-if="filter.dataType === 'DICT' && filter.cascadeDict"
                    v-model="filter.value"
                    placeholder="请选择"
                    expand-trigger="hover"
                    :options="allDictDatas[filter.key]"
                    :field-names="{ value: 'id', label: 'label' }"
                  />
                  <icon-minus-circle
                    class="remove-filter"
                    v-if="canRemove"
                    @click="handleRemoveFilter(index)"
                  />
                </a-form-item>
              </a-col>
            </a-row>
          </a-form>
        </div>
        <div v-if="conditions.groups.length > 0">
          <div v-for="(group, index) in conditions.groups" :key="index">
            <div class="group">
              <UserGroupConditions
                class="group-item"
                :conditions="group"
                :columns="allUserColumns"
                :dictDatas="allDictDatas"
                ref="groupRef"
              />
              <icon-minus-circle
                v-if="canRemove"
                class="remove-group"
                @click="handleRemoveGroup(index)"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
    <div
      :class="[
        'btn-container',
        conditions.filters?.length + conditions.groups?.length > 1
          ? 'btn-container-ml44'
          : '',
      ]"
    >
      <a-space>
        <a-button type="text" size="mini" @click="handleAddFilter"
          >添加筛选条件</a-button
        >
        <a-button type="text" size="mini" @click="handleAddGroup"
          >添加筛选组条件</a-button
        >
      </a-space>
    </div>
  </div>
</template>

<script setup lang="ts">
import {getEnabledDictData} from "@/api/dict";
import {getUserAttrs} from "@/api/user";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {computed, onMounted, ref, watch} from "vue";

const props = defineProps<{
  conditions: any;
  columns?: any[];
  dictDatas?: any;
}>();

const emit = defineEmits<{
  (e: "update:conditions", value: any): void;
}>();

const columnsLoaded = ref(false);
const formRef = ref();
const groupRef = ref();
const allUserColumns = [];
const allDictDatas = {};
const canRemove = computed(() => {
  return props.conditions.filters?.length + props.conditions.groups?.length > 1;
});

watch(
  () => props.conditions,
  (newVal) => {
    allUserColumns.forEach(async (item: any) => {
      if (item.dataType === "DICT" && item.dictId) {
        if (item.cascadeDict !== undefined) {
          setFilterCascadeDict(item.key, item.cascadeDict);
        }
      }
    });
    emit("update:conditions", newVal);
  },
  { deep: true, immediate: true }
);

const handleGetAllUserColumns = () => {
  getUserAttrs({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        allUserColumns.length = 0;
        allUserColumns.push(...data.list);

        const getEnabledDictDataPromises = [];
        allUserColumns.forEach(async (item: any) => {
          if (item.dataType === "DICT" && item.dictId) {
            if (item.cascadeDict !== undefined) {
              setFilterCascadeDict(item.key, item.cascadeDict);
            }

            allDictDatas[item.key] = [];
            getEnabledDictDataPromises.push(
              handleGetEnabledDictData(item.key, item.dictId)
            );
          }
        });

        if (getEnabledDictDataPromises.length > 0) {
          Promise.all(getEnabledDictDataPromises).then(() => {
            columnsLoaded.value = true;
          });
          return;
        }

        columnsLoaded.value = true;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户属性");
    });
};

const setFilterCascadeDict = (key: any, isCascadeDict: boolean) => {
  const filter = props.conditions.filters.find((item) => item.key === key);
  if (filter) {
    filter.cascadeDict = isCascadeDict;
  }
};

const handleGetEnabledDictData = async (attrKey: string, dictId: string) => {
  try {
    const result = await getEnabledDictData(dictId);
    handleApiSuccess(result, (data: any) => {
      allDictDatas[attrKey].length = 0;
      allDictDatas[attrKey].push(...data);
    });
  } catch (err: any) {
    handleApiError(err, "获取启用的字典数据");
  }
};

const handleUserColumnsSelectChange = (value: any) => {
  const column = allUserColumns.find((item) => item.key === value);
  if (column) {
    const filter = props.conditions.filters.find(
      (item) => item.key === column.key
    );
    filter.dataType = column.dataType;
    filter.extFlg = column.extFlg;
    filter.cascadeDict = column.cascadeDict;
    filter.value = undefined;
    filter.filterType = undefined;
  }
};

const handleAddFilter = () => {
  props.conditions.filters.push({
    key: undefined,
    dataType: "STRING",
    value: undefined,
    filterType: undefined,
    extFlg: undefined,
    cascadeDict: undefined,
  });
};

const handleRemoveFilter = (index: number) => {
  props.conditions.filters.splice(index, 1);
};

const handleSwapperConjunction = () => {
  props.conditions.conjunction =
    props.conditions.conjunction === "AND" ? "OR" : "AND";
};

const handleAddGroup = () => {
  if (!props.conditions.groups) {
    props.conditions.groups = [];
  }
  props.conditions.groups.push({
    conjunction: "AND",
    filters: [
      {
        key: undefined,
        dataType: "STRING",
        value: undefined,
        filterType: undefined,
        extFlg: undefined,
        cascadeDict: undefined,
      },
    ],
    groups: [],
  });
};

const handleRemoveGroup = (index: number) => {
  props.conditions.groups.splice(index, 1);
};

const handleValidateForm = async () => {
  const validateRes = await formRef.value.validate();
  let groupValidateRes = true;
  if (groupRef.value) {
    const groupValidateResArr = [];
    groupRef.value.forEach((item) => groupValidateResArr.push(item.validate()));
    groupValidateRes = (await Promise.all(groupValidateResArr)).every(
      (item) => item
    );
  }
  return validateRes === undefined && groupValidateRes;
};

onMounted(() => {
  if (!props.columns || props.columns.length === 0) {
    handleGetAllUserColumns();
  } else {
    allUserColumns.push(...props.columns);
    if (props.dictDatas) {
      Object.assign(allDictDatas, props.dictDatas);
    }
    columnsLoaded.value = true;
  }
});

defineExpose({
  validate: handleValidateForm,
});
</script>

<style scoped lang="scss">
.conditions-container {
  display: flex;
  align-items: stretch;

  .conjunction-container {
    display: flex;
    flex-direction: column;
    align-items: center;
    margin-right: 12px;
    min-height: 100%;

    .conjunction {
      font-size: 12px;
      color: var(--color-neutral-8);
      user-select: none;
      cursor: pointer;
      padding: 4px 0;
    }

    .conjunction-line-top,
    .conjunction-line-btm {
      flex: 1;
      width: 24px;
      border: 1px solid var(--color-neutral-4);
    }

    .conjunction-line-top {
      border-bottom: none;
      border-right: none;
      border-radius: 6px 0;
      border-top-left-radius: 8px;
      margin-bottom: 4px;
      margin-top: 16px;
    }

    .conjunction-line-btm {
      border-top: none;
      border-right: none;
      border-radius: 0 0 0 6px;
      border-bottom-left-radius: 8px;
      margin-top: 4px;
      margin-bottom: 34px;
    }
  }

  .conditions-content-container {
    flex: 1;
    min-height: 100%;

    .remove-filter {
      color: #86909c;
      cursor: pointer;
      margin-left: 8px;
      font-size: 16px;
    }

    .group {
      display: flex;
      align-items: center;
      width: 100%;

      .group-item {
        position: relative;
        padding: 16px;
        margin-bottom: 16px;
        border: 1px solid var(--color-neutral-4);
        border-radius: 4px;
        width: 100%;
      }

      .remove-group {
        color: #86909c;
        cursor: pointer;
        margin-left: 8px;
        font-size: 16px;
      }
    }
  }
}

.btn-container {
  margin-bottom: 14px;
}

.btn-container-ml44 {
  margin-left: 44px;
}
</style>
