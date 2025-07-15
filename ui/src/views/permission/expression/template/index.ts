import {deletePermissionExpTemplate, getPermissionExpTemplateList,} from "@/api/permission";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";

/**
 * 跳转创建模板
 */
const handleToCreateTemplate = () => {
  router.push({
    path: "/permission/expression/template/create",
  });
};

/**
 * 跳转模板详情
 */
const handleToTemplateDetail = (id: string) => {
  router.push({
    path: "/permission/expression/template/detail",
    query: {
      id,
    },
  });
};

/**
 * 模板列表
 */
const templateList = reactive([]);

/**
 * 模板名称搜索关键字
 */
const searchKeyword = ref(undefined);

/**
 * 获取模板列表
 */
const handleGetTemplateList = () => {
  getPermissionExpTemplateList({
    keyword: searchKeyword.value,
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        templateList.length = 0;
        templateList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限表达式模板列表");
    });
};

/**
 * 删除模板
 */
const handleDeleteTemplate = (template: any) => {
  Modal.confirm({
    title: `确定删除模板「${template.name}」及关联的权限表达式吗？`,
    content: "此操作将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deletePermissionExpTemplate(template.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetTemplateList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除权限表达式模板");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetTemplateList();
    });

    return {
      handleToCreateTemplate,
      searchKeyword,
      templateList,
      handleGetTemplateList,
      handleToTemplateDetail,
      handleDeleteTemplate,
    };
  },
});
