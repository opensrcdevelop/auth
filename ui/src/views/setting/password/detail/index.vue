<script lang="ts">
import indexTs from "./index";

export default indexTs;
</script>

<style lang="scss" scoped>
@use "./index.scss";
</style>

<template>
  <div>
    <page-header @back="handleBack">
      <div class="detail-title">{{ passwordPolicyName }}</div>
      <div class="info-title">基础信息</div>
      <a-form
        :model="basicInfoForm"
        :rules="basicInfoFormRules"
        ref="basicInfoFormRef"
        layout="vertical"
      >
        <a-form-item field="name" label="策略名称">
          <a-input v-model="basicInfoForm.name" placeholder="请输入策略名称" :disabled="passwordPolicyName === '默认策略'" />
        </a-form-item>
        <a-form-item field="desc" label="策略描述">
          <a-textarea
            v-model="basicInfoForm.desc"
            placeholder="请输入策略描述"
            :auto-size="{
              minRows: 3,
              maxRows: 5,
            }"
          />
        </a-form-item>
      </a-form>
      <div class="info-title">策略应用主体</div>
      <a-alert :show-icon="false" v-if="passwordPolicyName === '默认策略'" style="margin-bottom: 20px;">
        <template #title>
          全体用户
        </template>
      </a-alert>
      <a-form
        v-else
        :model="policyPrincipalForm"
        ref="policyPrincipalFormRef"
        :rules="policyPrincipalFormRules"
        layout="vertical"
      >
        <a-row :gutter="24">
          <a-col :span="12">
            <a-form-item field="type" label="主体类型">
              <a-radio-group
                v-model="policyPrincipalForm.type"
                @change="principalSelectChange"
              >
                <a-radio value="USER">用户</a-radio>
                <a-radio value="USER_GROUP">用户组</a-radio>
              </a-radio-group>
            </a-form-item>
          </a-col>
          <a-col :span="12">
            <a-form-item field="id" label="主体">
              <a-select
                v-if="policyPrincipalForm.type === 'USER'"
                placeholder="请选择用户"
                allow-clear
                allow-search
                multiple
                v-model:model-value="policyPrincipalForm.id"
                v-model:input-value="userSearchKeyword"
                @search="handleGetAllUsers"
                @clear="handleGetAllUsers"
                :filter-option="false"
              >
                <a-option
                  v-for="user in userList"
                  :key="user.userId"
                  :value="user.userId"
                >
                  {{ user.username }}
                </a-option>
              </a-select>
              <a-select
                v-if="policyPrincipalForm.type === 'USER_GROUP'"
                placeholder="请选择用户组"
                allow-clear
                allow-search
                multiple
                v-model:model-value="policyPrincipalForm.id"
                v-model:input-value="userGroupSearchKeyword"
                @search="handleGetAllUserGroups"
                @clear="handleGetAllUserGroups"
                :filter-option="false"
              >
                <a-option
                  v-for="userGroup in userGroupList"
                  :key="userGroup.id"
                  :value="userGroup.id"
                >
                  {{ userGroup.name }}
                </a-option>
              </a-select>
            </a-form-item>
          </a-col>
        </a-row>
      </a-form>
      <div class="info-title">密码强度</div>
      <a-form
        :model="passwordStrengthForm"
        ref="passwordStrengthFormRef"
        :rules="passwordStrengthFormRules"
        layout="vertical"
      >
        <a-form-item field="passwordStrength">
          <template #label>
            <div class="password-strength-label">
              <span>密码强度规则</span>
              <a-tag class="password-strength-tag" color="gray" size="small">{{
                passwordStrengthLabel
              }}</a-tag>
            </div>
          </template>
          <a-select
            v-model="passwordStrengthForm.passwordStrength"
            placeholder="请选择密码规则"
            @change="handleCheckPassword"
          >
            <a-option :value="0">用户可使用任意非空字符串作密码</a-option>
            <a-option :value="1">用户须使用至少 6 位字符作为密码</a-option>
            <a-option :value="2"
              >用户须使用至少 6
              位字符作为密码，须包含英文、数字与符号中的两种</a-option
            >
            <a-option :value="3"
              >用户须使用至少 6
              位字符作为密码，且密码中须包含英文、数字与符号</a-option
            >
            <a-option :value="4">自定义配置密码强度规则</a-option>
          </a-select>
        </a-form-item>
        <div v-if="passwordStrengthForm.passwordStrength === 4">
          <a-row :gutter="24">
            <a-col :span="4">
              <a-form-item field="minLength" label="密码最小位数">
                <a-input-number
                  :step="1"
                  :min="1"
                  :max="35"
                  mode="button"
                  v-model="passwordStrengthForm.minLength"
                />
              </a-form-item>
            </a-col>
            <a-col :span="4">
              <a-form-item field="maxLength" label="密码最大位数">
                <a-input-number
                  :step="1"
                  :min="1"
                  :max="35"
                  mode="button"
                  v-model="passwordStrengthForm.maxLength"
                />
              </a-form-item>
            </a-col>
            <a-col :span="16">
              <a-form-item field="charType" label="密码字符类型要求">
                <div class="select-char-type-container">
                  <a-checkbox-group
                    v-model="passwordStrengthForm.charType"
                    @change="() => (passwordStrengthForm.minCharTypeCount = 0)"
                  >
                    <a-checkbox value="UPPER_CASE"
                      >大写字母（A 到 Z）</a-checkbox
                    >
                    <a-checkbox value="LOWER_CASE"
                      >小写字母（a 到 z）</a-checkbox
                    >
                    <a-checkbox value="NUMBER">数字（0 到 9）</a-checkbox>
                    <a-checkbox value="SPECIAL_CHAR"
                      >特殊字符（例如：!@#%*.）</a-checkbox
                    >
                  </a-checkbox-group>
                  <div class="chat-type-count-select">
                    <div>至少需要满足以上字符类型中的</div>
                    <a-select
                      v-model="passwordStrengthForm.minCharTypeCount"
                      style="width: 80px; margin-left: 8px; margin-right: 8px"
                      :disabled="passwordStrengthForm.charType.length === 0"
                    >
                      <a-option
                        v-if="
                          passwordStrengthForm.charType.length === 0 ||
                          passwordStrengthForm.charType.length === 1
                        "
                        :value="0"
                      >
                        全部
                      </a-option>
                      <a-option
                        v-if="passwordStrengthForm.charType.length > 1"
                        v-for="(item, i) in passwordStrengthForm.charType"
                        :key="i"
                        :value="i"
                      >
                        {{ i === 0 ? "全部" : `${i} 种` }}
                      </a-option>
                    </a-select>
                  </div>
                </div>
              </a-form-item>
            </a-col>
          </a-row>
          <a-form-item label="禁止使用的密码类型配置">
            <div class="prohibit-password-type-container">
              <a-checkbox
                class="prohibit-password-type-checkbox"
                v-model="passwordStrengthForm.prohibitUserInfo"
                >禁止包含用户信息（手机号码、邮箱、用户名）</a-checkbox
              >
              <a-checkbox
                class="prohibit-password-type-checkbox"
                v-model="passwordStrengthForm.prohibitSingleChar"
                >禁止全部单一字符</a-checkbox
              >
              <a-checkbox
                class="prohibit-password-type-checkbox"
                v-model="passwordStrengthForm.prohibitConsecutiveChar"
                >禁止全部连续字符（例如：123456、abcdef）</a-checkbox
              >
              <a-checkbox
                class="prohibit-password-type-checkbox"
                v-model="passwordStrengthForm.prohibitContainConsecutiveChar"
              >
                <div style="display: flex; align-items: center">
                  <div>禁止包含</div>
                  <a-input-number
                    style="width: 80px; margin-left: 8px; margin-right: 8px"
                    :min="2"
                    :max="35"
                    :step="1"
                    mode="button"
                    size="mini"
                    v-model="passwordStrengthForm.minConsecutiveCharLength"
                    :disabled="
                      !passwordStrengthForm.prohibitContainConsecutiveChar
                    "
                  ></a-input-number>
                  <div>个连续字符</div>
                </div>
              </a-checkbox>
              <a-checkbox
                class="prohibit-password-type-checkbox"
                v-model="passwordStrengthForm.prohibitContainRepeatChar"
              >
                <div style="display: flex; align-items: center">
                  <div>禁止包含</div>
                  <a-input-number
                    style="width: 80px; margin-left: 8px; margin-right: 8px"
                    :min="2"
                    :max="35"
                    :step="1"
                    mode="button"
                    size="mini"
                    v-model="passwordStrengthForm.minRepeatCharLength"
                    :disabled="!passwordStrengthForm.prohibitContainRepeatChar"
                  ></a-input-number>
                  <div>个连续重复字符</div>
                </div>
              </a-checkbox>
              <div style="display: flex; align-items: center; height: 24px">
                <a-checkbox
                  v-model="passwordStrengthForm.prohibitSpecificPassword"
                  >禁止使用特定密码
                </a-checkbox>
                <a-button
                  type="text"
                  size="mini"
                  style="margin-left: 8px"
                  @click="() => (specificPasswordListEditModalVisible = true)"
                  >编辑</a-button
                >
              </div>
            </div>
          </a-form-item>
        </div>
        <a-form-item label="检查密码">
          <password-checker
            type="text"
            :loading="checkLoading"
            @check="handleCheckPassword"
            :checkRes="checkRes"
          />
        </a-form-item>
        <a-form-item label="用户登录密码强度检查">
          <a-switch
            type="round"
            v-model="passwordStrengthForm.enablePasswordDetection"
          />
          <template #extra>
            <div>
              开启后，用户登录时会对密码强度进行检查，不符合当前密码强度的用户会被引导修改密码后才可以登录
            </div>
          </template>
        </a-form-item>
      </a-form>
      <div class="info-title">密码轮换策略</div>
      <a-form
        :model="forceChangePasswordForm"
        ref="forceChangePasswordFormRef"
        :rules="forceChangePasswordFormRules"
        layout="vertical"
      >
        <a-form-item label="强制用户定期修改密码">
          <a-switch
            type="round"
            v-model="forceChangePasswordForm.enableForceChangePassword"
          />
          <template #extra>
            <div>开启后，超过选定周期，用户登录将强制修改密码</div>
          </template>
        </a-form-item>
        <div v-if="forceChangePasswordForm.enableForceChangePassword">
          <a-row :gutter="24">
            <a-col :span="12">
              <a-form-item field="forcedCycle" label="强制修改密码周期">
                <a-input-number
                  :min="1"
                  :max="99"
                  :step="1"
                  mode="button"
                  v-model="forceChangePasswordForm.forcedCycle"
                  style="width: 120px"
                />
                <a-select
                  v-model="forceChangePasswordForm.forcedCycleUnit"
                  style="margin-left: 24px; width: 80px"
                >
                  <a-option value="DAY">天</a-option>
                  <a-option value="MONTH">月</a-option>
                  <a-option value="YEAR">年</a-option>
                </a-select>
              </a-form-item>
            </a-col>
            <a-col :span="12">
              <a-form-item field="remindCycle" label="密码到期前提醒周期">
                <a-input-number
                  :min="1"
                  :max="99"
                  :step="1"
                  mode="button"
                  v-model="forceChangePasswordForm.remindCycle"
                  style="width: 120px"
                />
                <a-select
                  v-model="forceChangePasswordForm.remindCycleUnit"
                  style="margin-left: 24px; width: 80px"
                >
                  <a-option value="DAY">天</a-option>
                  <a-option value="MONTH">月</a-option>
                  <a-option value="YEAR">年</a-option>
                </a-select>
                <template #extra>
                  <div>
                    临期内每天给用户发送一次邮件提醒，直到用户成功修改密码。
                  </div>
                </template>
              </a-form-item>
            </a-col>
          </a-row>
        </div>
      </a-form>
      <a-space>
        <a-button type="primary" @click="handleUpdatePasswordPolicyFormSubmit"
          >保存</a-button
        >
        <a-button @click="handleResetUpdatePasswordPolicyForm">重置</a-button>
      </a-space>
    </page-header>

    <a-modal
      :visible="specificPasswordListEditModalVisible"
      @close="() => (specificPasswordListEditModalVisible = false)"
      @cancel="() => (specificPasswordListEditModalVisible = false)"
      :footer="false"
      title="编辑特定密码"
      width="620px"
    >
      <div class="edit-specific-password-container">
        <div class="list-container">
          <a-empty
            v-if="passwordStrengthForm.specificPasswordList.length === 0"
          />
          <div class="input-container" v-else>
            <div
              class="input-item"
              v-for="(item, i) in passwordStrengthForm.specificPasswordList"
              :key="i"
            >
              <a-input
                v-model="passwordStrengthForm.specificPasswordList[i]"
                placeholder="请输入密码"
              >
                <template #prefix> #{{ i + 1 }} </template>
              </a-input>
              <icon-minus-circle
                @click="handleDeleteSpecificPasswordInputItem(i)"
                style="
                  color: rgb(169, 174, 184);
                  cursor: pointer;
                  margin-left: 8px;
                "
              />
            </div>
          </div>
        </div>
        <div
          class="add-btn-container"
          @click="handleAddSpecificPasswordInputItem"
        >
          <icon-plus style="margin-right: 4px" />
          <span>添加密码</span>
        </div>
      </div>
    </a-modal>
  </div>
</template>
