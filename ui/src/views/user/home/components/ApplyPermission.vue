<template>
  <div class="apply-permission-container">
    <div class="permission-tree">
      <div class="tree-header">
        <div class="section-title">选择要申请的权限</div>
        <div class="select-all-wrapper" v-if="filteredTreeData.length > 0">
          <a-checkbox
            :model-value="isAllSelected"
            :indeterminate="isIndeterminate"
            @change="toggleSelectAll"
          >
            {{ isAllSelected ? "取消全选" : "全选" }}
          </a-checkbox>
          <span class="select-count" v-if="selectedPermissionIds.length > 0">
            已选择 {{ selectedPermissionIds.length }} 个权限
          </span>
        </div>
      </div>
      <div class="tree-tip">
        <icon-info-circle style="margin-right: 4px" />
        <span>已拥有或审批中的权限不可选择</span>
      </div>
      <a-input-search
        v-model="searchKeyword"
        placeholder="输入权限名称或标识进行搜索"
        style="margin-bottom: 12px; width: 280px"
        allow-clear
        @search="handleSearch"
        @press-enter="handleSearch(searchKeyword)"
      />
      <div class="tree-content" v-if="filteredTreeData.length > 0">
        <div class="tree-left">
          <a-tree
            v-model:checked-keys="displayCheckedKeys"
            :data="filteredTreeData"
            :checkable="true"
            :checkStrictly="true"
            :show-line="true"
            :field-names="{ key: 'id', title: 'name' }"
            style="max-height: 400px; overflow-y: auto"
            @check="handleTreeCheck"
          >
            <template #title="nodeData">
              <div @click="handleNodeClick(nodeData)">
                <span class="node-title">{{ nodeData.name }}</span>
                <a-tag
                  v-if="nodeData?.owned"
                  color="gray"
                  size="small"
                  style="margin-left: 4px"
                >
                  已拥有
                </a-tag>
                <a-tag
                  v-if="nodeData?.pending"
                  color="arcoblue"
                  size="small"
                  style="margin-left: 4px"
                >
                  待审批
                </a-tag>
                <a-tag
                  v-if="
                    nodeData?.autoApprove &&
                    !nodeData?.owned &&
                    !nodeData?.pending
                  "
                  color="green"
                  size="small"
                  style="margin-left: 4px"
                >
                  自动批准
                </a-tag>
              </div>
            </template>
          </a-tree>
        </div>
        <div class="tree-right" v-if="selectedPermissionDetails.length > 0">
          <div class="selected-header">已选权限</div>
          <div class="selected-list">
            <div
              class="selected-item"
              v-for="item in selectedPermissionDetails"
              :key="item.id"
            >
              <div class="selected-item-info">
                <div class="selected-item-name">{{ item.name }}</div>
                <div class="selected-item-path">{{ item.path }}</div>
              </div>
              <icon-close
                class="remove-icon"
                @click="removeSelectedPermission(item.id)"
              />
            </div>
          </div>
        </div>
      </div>
      <a-empty v-else description="暂无可申请的权限" />
    </div>

    <!-- 申请表单区域 -->
    <div class="apply-form">
      <div class="section-title">申请理由</div>
      <a-textarea
        v-model="reason"
        placeholder="请输入申请理由，说明为什么需要这些权限"
        :max-length="500"
        show-word-limit
        :auto-size="{
          minRows: 3,
          maxRows: 5,
        }"
        style="margin-bottom: 16px; margin-top: 16px"
      />
      <div class="submit-container">
        <a-button type="primary" :disabled="!canSubmit" @click="handleSubmit">
          提交申请
        </a-button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import {computed, ref, watch} from "vue";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {getAvailablePermissionTree, submitPermissionRequest,} from "@/api/permission";

const props = withDefaults(
  defineProps<{
    activeKey?: string;
  }>(),
  {
    activeKey: "",
  },
);

const emit = defineEmits<{
  (e: "submit-success"): void;
}>();

interface PermissionTreeNode {
  id: string;
  name: string;
  code: string;
  type: "RESOURCE_GROUP" | "RESOURCE" | "PERMISSION";
  autoApprove: boolean;
  pending: boolean;
  owned: boolean;
  children?: PermissionTreeNode[];
  disabled?: boolean;
  checkable?: boolean;
}

const permissionTree = ref<PermissionTreeNode[]>([]);
const treeData = ref<PermissionTreeNode[]>([]);
const filteredTreeData = ref<PermissionTreeNode[]>([]);
const reason = ref("");
const searchKeyword = ref("");

// 用于显示的勾选状态（包含父节点）
const displayCheckedKeys = ref<string[]>([]);
// 实际选中的权限ID（只包含权限叶子节点）
const selectedPermissionIds = ref<string[]>([]);

// 获取节点下所有可选择的权限ID
const getSelectablePermissionIds = (node: PermissionTreeNode): string[] => {
  const ids: string[] = [];
  if (node.type === "PERMISSION" && !node.owned && !node.pending) {
    ids.push(node.id);
  }

  if (node.children) {
    for (const child of node.children) {
      ids.push(...getSelectablePermissionIds(child));
    }
  }
  return ids;
};

// 获取节点下所有可选择的权限节点
const getSelectablePermissionNodes = (
  node: PermissionTreeNode,
): PermissionTreeNode[] => {
  const nodes: PermissionTreeNode[] = [];
  if (node.type === "PERMISSION" && !node.owned && !node.pending) {
    nodes.push(node);
  }
  if (node.children) {
    for (const child of node.children) {
      nodes.push(...getSelectablePermissionNodes(child));
    }
  }
  return nodes;
};

// 根据节点ID查找节点
const findNodeById = (
  nodes: PermissionTreeNode[],
  id: string,
): PermissionTreeNode | null => {
  for (const node of nodes) {
    if (node.id === id) return node;
    if (node.children) {
      const found = findNodeById(node.children, id);
      if (found) return found;
    }
  }
  return null;
};

// 所有可选择的权限节点
const allSelectablePermissions = computed(() => {
  const result: PermissionTreeNode[] = [];
  for (const node of treeData.value) {
    result.push(...getSelectablePermissionNodes(node));
  }
  return result;
});

// 全选/取消全选
const isAllSelected = computed(() => {
  return (
    selectedPermissionIds.value.length ===
      allSelectablePermissions.value.length &&
    allSelectablePermissions.value.length > 0
  );
});

const isIndeterminate = computed(() => {
  return (
    selectedPermissionIds.value.length > 0 &&
    selectedPermissionIds.value.length < allSelectablePermissions.value.length
  );
});

const toggleSelectAll = () => {
  if (isAllSelected.value) {
    selectedPermissionIds.value = [];
    displayCheckedKeys.value = [];
  } else {
    selectedPermissionIds.value = allSelectablePermissions.value.map(
      (p) => p.id,
    );
    // 更新显示的勾选状态：所有父节点都勾选
    updateDisplayCheckedKeys();
  }
};

// 更新显示的勾选状态
const updateDisplayCheckedKeys = () => {
  const checkedSet = new Set(selectedPermissionIds.value);
  const result: string[] = [];

  const checkParents = (nodes: PermissionTreeNode[]) => {
    for (const node of nodes) {
      if (node.type !== "PERMISSION") {
        // 检查是否有任何子权限被选中
        const childPermissionIds = getSelectablePermissionIds(node);
        const hasCheckedChild = childPermissionIds.some((id) =>
          checkedSet.has(id),
        );
        if (hasCheckedChild) {
          result.push(node.id);
        }
      }
      if (node.children) {
        checkParents(node.children);
      }
    }
  };

  checkParents(treeData.value);
  displayCheckedKeys.value = [...result, ...selectedPermissionIds.value];
};

// 处理树节点勾选变化
const handleTreeCheck = (
  allCheckedKeys: string[],
  extra: { checked: boolean; node: any },
) => {
  const { checked, node: nodeData } = extra;
  if (!nodeData) return;

  const nodeId = nodeData.id;
  const node = findNodeById(treeData.value, nodeId);
  if (!node) return;

  // 禁用的节点不处理
  if (node.disabled) return;

  if (node.type === "PERMISSION") {
    // 直接勾选/取消权限节点
    if (checked) {
      if (!selectedPermissionIds.value.includes(nodeId)) {
        selectedPermissionIds.value.push(nodeId);
      }
    } else {
      selectedPermissionIds.value = selectedPermissionIds.value.filter(
        (id) => id !== nodeId,
      );
    }
  } else {
    // 勾选/取消父节点（资源组或资源）
    const childPermissionIds = getSelectablePermissionIds(node);
    if (checked) {
      // 添加所有子权限
      for (const id of childPermissionIds) {
        if (!selectedPermissionIds.value.includes(id)) {
          selectedPermissionIds.value.push(id);
        }
      }
    } else {
      // 移除所有子权限
      selectedPermissionIds.value = selectedPermissionIds.value.filter(
        (id) => !childPermissionIds.includes(id),
      );
    }
  }

  // 更新显示状态
  updateDisplayCheckedKeys();
};

// 点击节点标题切换选中状态
const handleNodeClick = (nodeData: any) => {
  const node = findNodeById(treeData.value, nodeData.id);
  if (!node || node.disabled) return;

  // 判断当前是否选中
  const isChecked = displayCheckedKeys.value.includes(node.id);

  // 触发与当前状态相反的操作
  handleTreeCheck([], { checked: !isChecked, node: nodeData });
};

// 已选择的权限详情（包含路径）
const selectedPermissionDetails = computed(() => {
  const details: { id: string; name: string; path: string }[] = [];

  const findPath = (
    nodes: PermissionTreeNode[],
    targetId: string,
    path: string[],
  ): string | null => {
    for (const node of nodes) {
      // 只有非权限节点才加入路径
      const currentPath =
        node.type !== "PERMISSION" ? [...path, node.name] : path;

      if (node.id === targetId) {
        // 权限节点返回当前路径（即父路径）
        return currentPath.join(" / ");
      }
      if (node.children && node.children.length > 0) {
        const found = findPath(node.children, targetId, currentPath);
        if (found) return found;
      }
    }
    return null;
  };

  for (const id of selectedPermissionIds.value) {
    const parentPath = findPath(treeData.value, id, []);
    if (parentPath !== null) {
      const node = findNodeById(treeData.value, id);
      details.push({ id, name: node ? node.name : id, path: parentPath });
    }
  }

  return details;
});

// 移除已选择的权限
const removeSelectedPermission = (id: string) => {
  selectedPermissionIds.value = selectedPermissionIds.value.filter(
    (key) => key !== id,
  );
  updateDisplayCheckedKeys();
};

// 将权限树转换为 a-tree 需要的数据格式
const transformToTreeData = (
  nodes: PermissionTreeNode[],
): PermissionTreeNode[] => {
  return nodes.map((node) => {
    const isPermission = node.type === "PERMISSION";
    // 权限节点且 owned 或 pending 时禁用
    const shouldDisabled = isPermission && (!!node.owned || !!node.pending);

    node.disabled = shouldDisabled;
    node.checkable = true;

    if (node.children && node.children.length > 0) {
      transformToTreeData(node.children);
    }

    return node;
  });
};

// 过滤树数据（根据搜索关键词）
const filterTreeData = (keyword: string) => {
  if (!keyword.trim()) {
    filteredTreeData.value = treeData.value;
    return;
  }

  const lowerKeyword = keyword.toLowerCase();

  const filterNode = (nodes: PermissionTreeNode[]): PermissionTreeNode[] => {
    const result: PermissionTreeNode[] = [];

    for (const node of nodes) {
      // 检查当前节点是否匹配
      const matchSelf =
        node.name.toLowerCase().includes(lowerKeyword) ||
        node.code.toLowerCase().includes(lowerKeyword);

      // 递归过滤子节点
      const filteredChildren = node.children ? filterNode(node.children) : [];

      // 如果当前节点匹配或者有匹配的子节点，则保留
      if (matchSelf || filteredChildren.length > 0) {
        result.push({
          ...node,
          children: filteredChildren,
        });
      }
    }

    return result;
  };

  filteredTreeData.value = filterNode(treeData.value);
};

// 处理搜索输入
const handleSearch = (value: string | number) => {
  filterTreeData(String(value));
};

// 加载可申请权限树
const handleLoadPermissionTree = () => {
  getAvailablePermissionTree()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissionTree.value = data || [];
        treeData.value = transformToTreeData(data || []);
        filteredTreeData.value = treeData.value;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取可申请权限");
    });
};

// 提交按钮是否可用
const canSubmit = computed(() => {
  return (
    selectedPermissionIds.value.length > 0 && reason.value.trim().length > 0
  );
});

// 提交权限申请
const handleSubmit = () => {
  if (!canSubmit.value) return;

  submitPermissionRequest({
    permissionIds: selectedPermissionIds.value,
    reason: reason.value.trim(),
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("权限申请提交成功");
        selectedPermissionIds.value = [];
        displayCheckedKeys.value = [];
        searchKeyword.value = "";
        reason.value = "";
        handleLoadPermissionTree();
        emit("submit-success");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "提交权限申请");
    });
};

/**
 * 执行初始化
 */
const handleInit = () => {
  handleLoadPermissionTree();
};

/**
 * 监听 tab 切换，当激活当前 tab 时才执行初始化
 */
watch(
  () => props.activeKey,
  (newActiveKey) => {
    if (newActiveKey === "apply_permission") {
      handleInit();
    }
  },
  { immediate: true },
);
</script>

<style lang="scss" scoped>
.apply-permission-container {
  padding: 16px;

  .section-title {
    font-size: 16px;
    font-weight: 500;
    color: #1d2129;
  }

  .permission-tree {
    background: #fff;
    padding: 16px;
    border-radius: 4px;
    margin-bottom: 16px;

    .tree-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 12px;

      .select-all-wrapper {
        display: flex;
        align-items: center;
        gap: 16px;

        .select-count {
          font-size: 14px;
          color: #1650d8;
          font-weight: 500;
        }
      }
    }

    .tree-tip {
      font-size: 12px;
      color: #86909c;
      margin-bottom: 12px;
      display: flex;
      align-items: center;
    }

    .tree-content {
      display: flex;
      gap: 16px;

      .tree-left {
        flex: 1;
        min-width: 0;
      }

      .tree-right {
        width: 300px;
        border-left: 1px solid #e5e6e8;
        padding-left: 16px;

        .selected-header {
          font-size: 14px;
          font-weight: 500;
          color: #1d2129;
          margin-bottom: 12px;
        }

        .selected-list {
          max-height: 400px;
          overflow-y: auto;
          padding-right: 2px;

          .selected-item {
            display: flex;
            align-items: flex-start;
            justify-content: space-between;
            padding: 8px 12px;
            background: #f2f3f5;
            border-radius: 4px;
            margin-bottom: 8px;

            &:last-child {
              margin-bottom: 0;
            }

            .selected-item-info {
              flex: 1;
              min-width: 0;

              .selected-item-name {
                font-size: 14px;
                color: #1d2129;
                font-weight: 500;
              }

              .selected-item-path {
                font-size: 12px;
                color: #86909c;
                margin-top: 4px;
              }
            }

            .remove-icon {
              cursor: pointer;
              color: #86909c;
              flex-shrink: 0;
              margin-left: 8px;

              &:hover {
                color: #0d419d;
              }
            }
          }
        }
      }
    }

    :deep(.arco-tree-node-selected .arco-tree-node-title) {
      color: inherit;
    }
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

    .submit-container {
      display: flex;
      justify-content: flex-end;
    }
  }
}
</style>
