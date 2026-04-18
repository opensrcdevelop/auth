<template>
  <a-card title="个人信息">
    <template #extra>
      <a-button type="text" @click="handleUpdateMyUserInfo">
        <template #icon>
          <icon-save />
        </template>
        保存
      </a-button>
    </template>
    <a-form :model="userAttrs" layout="vertical">
      <a-row :gutter="24">
        <a-col :span="12" v-for="attr in userAttrs" :key="attr.key">
          <a-form-item :label="attr.name">
            <a-input-number
              v-if="attr.dataType === 'NUMBER'"
              hide-button
              v-model="userInfo[attr.key]"
              :allowClear="attr.userEditable"
              :disabled="!attr.userEditable"
              :placeholder="`请输入${attr.name}`"
            />
            <a-input
              v-if="attr.dataType === 'STRING'"
              v-model="userInfo[attr.key]"
              :allowClear="attr.userEditable"
              :disabled="!attr.userEditable"
              :placeholder="`请输入${attr.name}`"
            />
            <a-select
              v-if="attr.dataType === 'BOOLEAN'"
              v-model="userInfo[attr.key]"
              :allowClear="attr.userEditable"
              :disabled="!attr.userEditable"
              :placeholder="`请选择${attr.name}`"
            >
              <a-option :value="true">是</a-option>
              <a-option :value="false">否</a-option>
            </a-select>
            <a-date-picker
              style="width: 100%"
              v-if="attr.dataType === 'DATETIME'"
              show-time
              value-format="timestamp"
              v-model="userInfo[attr.key]"
              :disabled="!attr.userEditable"
              :placeholder="`请选择${attr.name}`"
            />
            <a-date-picker
              style="width: 100%"
              v-if="attr.dataType === 'DATE'"
              value-format="timestamp"
              v-model="userInfo[attr.key]"
              :disabled="!attr.userEditable"
              :placeholder="`请选择${attr.name}`"
            />
            <a-select
              v-if="attr.dataType === 'DICT' && !attr.cascadeDict"
              v-model="userInfo[attr.key]"
              allow-clear
              allow-search
              :disabled="!attr.userEditable"
              :placeholder="`请选择${attr.name}`"
            >
              <a-option
                :value="dictData.id"
                v-for="dictData in allDictDatas[attr.key]"
                :key="dictData.id"
                >{{ dictData.label }}</a-option
              >
            </a-select>
            <a-cascader
              v-if="attr.dataType === 'DICT' && attr.cascadeDict"
              v-model="userInfo[attr.key]"
              :placeholder="`请选择${attr.name}`"
              expand-trigger="hover"
              :options="allDictDatas[attr.key]"
              :field-names="{ value: 'id', label: 'label' }"
              allow-clear
              allow-search
            />
          </a-form-item>
        </a-col>
      </a-row>
    </a-form>
  </a-card>
</template>

<script setup lang="ts">
import {getEnabledDictData} from "@/api/dict";
import {getCurrentUser, getVisibleUserAttrs, updateMyUserInfo,} from "@/api/user";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Message, Notification} from "@arco-design/web-vue";
import {reactive, watch} from "vue";

const props = withDefaults(
  defineProps<{
    userInfo: any;
    activeKey?: string;
  }>(),
  {
    userInfo: () => ({}),
    activeKey: "",
  },
);

const emit = defineEmits<{
  (e: "userInfoUpdated", data: any): void;
}>();

const userInfo = reactive(props.userInfo);

/** 用户属性 */
const userAttrs = reactive([] as any[]);

/** 字典数据值 */
const allDictDatas = reactive({} as any);

/**
 * 获取用户属性
 */
const handleGetUserAttrs = async () => {
  await getVisibleUserAttrs()
    .then((result: any) => {
      handleApiSuccess(result, async (data: any) => {
        userAttrs.length = 0;
        userAttrs.push(...data);

        // 将用户 ID 置为第一个属性
        const userIdIndex = userAttrs.findIndex(
          (item: any) => item.key === "userId",
        );
        if (userIdIndex > -1) {
          userAttrs.splice(0, 0, userAttrs.splice(userIdIndex, 1)[0]);
        }

        // 将用户名置为第二个属性
        const userNameIndex = userAttrs.findIndex(
          (item: any) => item.key === "username",
        );
        if (userNameIndex > -1) {
          userAttrs.splice(1, 0, userAttrs.splice(userNameIndex, 1)[0]);
        }

        // 将邮箱置为第三个属性
        const emailIndex = userAttrs.findIndex(
          (item: any) => item.key === "emailAddress",
        );
        if (emailIndex > -1) {
          userAttrs.splice(2, 0, userAttrs.splice(emailIndex, 1)[0]);
        }

        // 将手机号置为第四个属性
        const phoneIndex = userAttrs.findIndex(
          (item: any) => item.key === "phoneNumber",
        );
        if (phoneIndex > -1) {
          userAttrs.splice(3, 0, userAttrs.splice(phoneIndex, 1)[0]);
        }
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取可见的用户属性");
    });

  handleGetAllEnabledDictData();
};

/**
 * 获取所有启用的字典数据
 */
const handleGetAllEnabledDictData = async () => {
  const getEnabledDictDataPromises = [] as any[];
  userAttrs.forEach((item: any) => {
    if (item.dataType === "DICT" && item.dictId) {
      allDictDatas[item.key] = [];
      getEnabledDictDataPromises.push(
        handleGetEnabledDictData(item.key, item.dictId),
      );
    }
  });

  if (getEnabledDictDataPromises.length > 0) {
    await Promise.all(getEnabledDictDataPromises);
  }
};

/**
 * 获取启用的字典数据
 */
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

/**
 * 执行初始化
 */
const handleInit = () => {
  handleGetUserAttrs();
};

/**
 * 更新个人信息
 */
const handleUpdateMyUserInfo = () => {
  if (!userInfo["username"] || userInfo["username"].trim() === "") {
    Message.warning("用户名不能为空");
    return;
  }
  updateMyUserInfo(userInfo)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        // 获取最新用户信息并通知父组件更新
        getCurrentUser()
          .then((res: any) => {
            handleApiSuccess(res, (data: any) => {
              Object.assign(userInfo, data);
              emit("userInfoUpdated", data);
            });
          })
          .catch((err: any) => {
            handleApiError(err, "获取用户信息");
          });
        // 重新获取用户属性
        handleGetUserAttrs();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新个人信息");
    });
};

/**
 * 监听 tab 切换，当激活当前 tab 时才执行初始化
 */
watch(
  () => props.activeKey,
  (newActiveKey) => {
    if (newActiveKey === "user_info") {
      handleInit();
    }
  },
  { immediate: true },
);
</script>

<style scoped lang="scss"></style>
