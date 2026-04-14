<script lang="ts">
import {defineComponent, onMounted, ref} from 'vue';
import {getMyPermissions} from '@/api/userPermissionRequest';
import {handleApiSuccess, handleApiError} from '@/util/tool';

export default defineComponent({
  name: 'MyPermissions',
  setup() {
    const loading = ref(false);
    const permissions = ref([]);

    // 加载用户权限列表
    const handleLoadPermissions = () => {
      loading.value = true;
      getMyPermissions()
          .then((result: any) => {
            handleApiSuccess(result, (data: any) => {
              permissions.value = data || [];
            });
          })
          .catch((err: any) => {
            handleApiError(err, '获取我的权限');
          })
          .finally(() => {
            loading.value = false;
          });
    };

    onMounted(() => {
      handleLoadPermissions();
    });

    return {
      loading,
      permissions,
    };
  },
});
</script>

<template>
  <a-spin :loading="loading" style="width: 100%">
    <a-table
        :data="permissions"
        :bordered="false"
        :pagination="{pageSize: 10}"
        :empty-text="'暂无数据'"
    >
      <template #columns>
        <a-table-column title="权限名称" data-index="permissionName" ellipsis tooltip/>
        <a-table-column title="权限标识" data-index="permissionCode" ellipsis tooltip/>
        <a-table-column title="资源名称" data-index="resourceName" ellipsis tooltip/>
      </template>
    </a-table>
  </a-spin>
</template>
