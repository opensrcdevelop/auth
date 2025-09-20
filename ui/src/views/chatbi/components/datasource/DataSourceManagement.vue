<template>
  <div class="header">
    <a-input-search
      :style="{ width: '320px' }"
      placeholder="输入数据源名称进行搜索"
      allow-clear
      v-model="dataSourceSearchKeyword"
      @search="handleGetDataSourceList(1, 15)"
      @keyup.enter.native="handleGetDataSourceList(1, 15)"
      @clear="handleGetDataSourceList(1, 15)"
    />
    <a-button type="primary" @click="handleToCreateDataSource"
      >创建数据源</a-button
    >
  </div>
  <div class="datasource-list">
    <a-table
      :data="dataSourceList"
      :bordered="false"
      :scroll="{ y: '100%' }"
      :pagination="dataSourceListPagination.pagination"
      @page-change="dataSourceListPagination.handlePageChange"
      @page-size-change="dataSourceListPagination.handlePageSizeChange"
    >
      <template #columns>
        <a-table-column
          title="数据源名称"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span
              class="table-column-name"
              @click="handleToDataSourceDetail(record)"
            >
              {{ record.name }}
            </span>
          </template>
        </a-table-column>
        <a-table-column
          title="数据源类型"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span>
              {{ record.type }}
            </span>
          </template>
        </a-table-column>
         <a-table-column
          title="数据源描述"
          ellipsis
          tooltip
        >
          <template #cell="{ record }">
            <span>
              {{ record.desc ? record.desc : "-" }}
            </span>
          </template>
        </a-table-column>
        <a-table-column title="是否启用">
          <template #cell="{ record }">
            <a-switch
              type="round"
              size="small"
              v-model="record.enabled"
              @change="handleUpdateDataSourceState(record)"
            />
          </template>
        </a-table-column>
        <a-table-column
          title="表数量"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span>
              {{ record.tableCount ? record.tableCount : "-" }}
            </span>
          </template>
        </a-table-column>
        <a-table-column
          title="最后同步表时间"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span>
              {{ record.lastSyncTableTime ? record.lastSyncTableTime : "-" }}
            </span>
          </template>
        </a-table-column>
        <a-table-column
          title="同步表次数"
          ellipsis
          tooltip
          :sortable="{
            sortDirections: ['ascend', 'descend'],
          }"
        >
          <template #cell="{ record }">
            <span>
              {{ record.syncTableCount ? record.syncTableCount : "-" }}
            </span>
          </template>
        </a-table-column>
        <a-table-column title="操作" fixed="right" :width="60">
          <template #cell="{ record }">
            <a-dropdown>
              <a-button type="text">
                <template #icon>
                  <icon-more />
                </template>
              </a-button>
              <template #content>
                <a-doption
                  style="color: #545968"
                  @click="handleSyncTable(record)"
                >
                  <template #icon>
                    <icon-sync />
                  </template>
                  同步表</a-doption
                >
                <a-doption
                  style="color: #e8353e"
                  @click="handleDeleteDataSource(record)"
                >
                  <template #icon>
                    <icon-delete />
                  </template>
                  删除</a-doption
                >
              </template>
            </a-dropdown>
          </template>
        </a-table-column>
      </template>
    </a-table>
  </div>
</template>
<script setup lang="ts">
import {deleteDataSourceConf, getDataSourceConfList, syncTable, updateDataSourceConf,} from "@/api/chatbi";
import {usePagination} from "@/hooks/usePagination";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {reactive, ref} from "vue";

/** 数据源列表 */
const dataSourceList = reactive([]);
const dataSourceSearchKeyword = ref(null);
let dataSourceListPagination = usePagination(
  "dataSourceList",
  ({ page, size }) => {
    handleGetDataSourceList(page, size);
  }
);

/**
 * 获取数据源列表
 */
const handleGetDataSourceList = (page: number = 1, size: number = 15) => {
  getDataSourceConfList({
    page,
    size,
    keyword: dataSourceSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        dataSourceList.length = 0;
        dataSourceList.push(...data.list);

        dataSourceListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取数据源列表");
    });
};

/**
 * 跳转到数据源详情
 */
const handleToDataSourceDetail = (dataSource: any) => {
  router.push({
    path: "/chatbi/datasource/detail",
    query: {
      id: dataSource.id,
      active_tab: "data_source_info",
    },
  });
};

/**
 * 跳转到创建数据源
 */
const handleToCreateDataSource = () => {
  router.push({
    path: "/chatbi/datasource/create",
  });
};

/**
 * 同步表
 */
const handleSyncTable = (dataSource: any) => {
  syncTable(dataSource.id)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("操作成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "同步表");
    });
};

/**
 * 更新数据源启用状态
 */
const handleUpdateDataSourceState = (dataSource: any) => {
  updateDataSourceConf({
    id: dataSource.id,
    enabled: dataSource.enabled,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("更新数据源启用状态成功");
        handleGetDataSourceList();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新数据源启用状态");
    });
};

/**
 * 删除数据源
 */
const handleDeleteDataSource = (dataSource: any) => {
  Modal.warning({
    title: `确定删除数据源「${dataSource.name}」吗？`,
    content: "此操作将删除该数据源及包含的所有表，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteDataSourceConf(dataSource.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetDataSourceList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除资源组");
        });
    },
  });
};

/**
 * 初始化
 */
const init = () => {
  handleGetDataSourceList(
    dataSourceListPagination.pagination.current,
    dataSourceListPagination.pagination.pageSize
  );
};

defineExpose({
  init,
});
</script>

<style scoped lang="scss">
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.datasource-list {
  margin-top: 16px;
}

.table-column-name {
  cursor: pointer;
  color: #215ae5;
}
</style>
