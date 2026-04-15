<script lang="ts">
import {defineComponent, onMounted, ref, computed} from 'vue';
import {getAvailablePermissionTree, submitPermissionRequest} from '@/api/userPermissionRequest';
import {handleApiSuccess, handleApiError} from '@/util/tool';
import {Notification} from '@arco-design/web-vue';

export default defineComponent({
  name: 'ApplyPermission',
  setup() {
    const loading = ref(false);
    const submitting = ref(false);
    const permissionTree = ref([]);
    const selectedPermissionIds = ref(new Set<string>());
    const reason = ref('');

    // 收集所有权限节点（用于渲染）
    const allPermissionNodes = computed(() => {
      const nodes: any[] = [];
      permissionTree.value.forEach((rg: any) => {
        rg.resources?.forEach((res: any) => {
          res.permissions?.forEach((perm: any) => {
            nodes.push({
              ...perm,
              resourceGroupName: rg.resourceGroupName,
              resourceName: res.resourceName,
            });
          });
        });
      });
      return nodes;
    });

    // 加载可申请权限树
    const handleLoadPermissionTree = () => {
      loading.value = true;
      getAvailablePermissionTree()
          .then((result: any) => {
            handleApiSuccess(result, (data: any) => {
              permissionTree.value = data || [];
            });
          })
          .catch((err: any) => {
            handleApiError(err, '获取可申请权限');
          })
          .finally(() => {
            loading.value = false;
          });
    };

    // 复选框选择处理
    const handleCheckboxChange = (permissionId: string, checked: boolean) => {
      if (checked) {
        selectedPermissionIds.value.add(permissionId);
      } else {
        selectedPermissionIds.value.delete(permissionId);
      }
    };

    // 判断权限是否已选中
    const isPermissionSelected = (permissionId: string) => {
      return selectedPermissionIds.value.has(permissionId);
    };

    // 提交申请
    const handleSubmit = () => {
      if (selectedPermissionIds.value.size === 0 || !reason.value.trim()) {
        Notification.warning('请选择要申请的权限并填写理由');
        return;
      }

      submitting.value = true;
      submitPermissionRequest({
        permissionIds: Array.from(selectedPermissionIds.value),
        reason: reason.value.trim(),
      })
          .then((result: any) => {
            handleApiSuccess(result, () => {
              Notification.success('申请提交成功');
              // 清空选择和理由
              selectedPermissionIds.value.clear();
              reason.value = '';
              // 重新加载权限树（更新 alreadyGranted 状态）
              handleLoadPermissionTree();
            });
          })
          .catch((err: any) => {
            handleApiError(err, '提交申请');
          })
          .finally(() => {
            submitting.value = false;
          });
    };

    // 提交按钮是否可用
    const canSubmit = computed(() => {
      return selectedPermissionIds.value.size > 0 && reason.value.trim().length > 0;
    });

    onMounted(() => {
      handleLoadPermissionTree();
    });

    return {
      loading,
      submitting,
      permissionTree,
      selectedPermissionIds,
      reason,
      handleCheckboxChange,
      isPermissionSelected,
      handleSubmit,
      canSubmit,
      allPermissionNodes,
    };
  },
});
</script>

<template>
  <a-spin :loading="loading" style="width: 100%">
    <div class="apply-permission-container">
      <!-- 权限树展示 -->
      <div class="permission-tree">
        <a-table
            :data="allPermissionNodes"
            :bordered="false"
            :pagination="{pageSize: 20}"
        >
          <template #columns>
            <a-table-column title="资源组" data-index="resourceGroupName" :width="150"/>
            <a-table-column title="资源" data-index="resourceName" :width="150"/>
            <a-table-column title="权限名称" data-index="permissionName"/>
            <a-table-column title="权限标识" data-index="permissionCode" :width="200"/>
            <a-table-column title="操作" :width="100" align="center">
              <template #cell="{ record }">
                <a-checkbox
                    v-if="!record.alreadyGranted"
                    :model-value="isPermissionSelected(record.permissionId)"
                    @change="(val: boolean) => handleCheckboxChange(record.permissionId, val)"
                >
                  申请
                </a-checkbox>
                <a-tag v-else color="gray" style="cursor: not-allowed;">
                  已拥有
                </a-tag>
              </template>
            </a-table-column>
          </template>
        </a-table>
      </div>

      <!-- 申请理由和提交 -->
      <div class="apply-form">
        <a-divider />
        <div class="selected-info">
          已选择 <a-badge :count="selectedPermissionIds.size" :max-count="99"/> 个权限
        </div>
        <a-form layout="vertical">
          <a-form-item label="申请理由" required>
            <a-textarea
                v-model="reason"
                placeholder="请输入申请理由"
                :max-length="200"
                show-word-limit
                :auto-size="{ minRows: 3, maxRows: 5 }"
            />
          </a-form-item>
          <a-form-item hide-label>
            <a-button
                type="primary"
                html-type="submit"
                :disabled="!canSubmit"
                :loading="submitting"
                @click="handleSubmit"
            >
              提交申请
            </a-button>
          </a-form-item>
        </a-form>
      </div>
    </div>
  </a-spin>
</template>

<style lang="scss" scoped>
.apply-permission-container {
  .permission-tree {
    margin-bottom: 16px;
  }

  .apply-form {
    background: #fff;
    padding: 16px;
    border-radius: 4px;

    .selected-info {
      margin-bottom: 16px;
      font-size: 14px;
      color: #515b78;
    }
  }
}
</style>
