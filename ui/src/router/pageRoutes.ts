import {RouteRecordRaw} from "vue-router";

const pageRoutes: RouteRecordRaw[] = [
  {
    path: "/client/detail",
    component: () => import("@/views/client/detail/index.vue"),
    meta: {
      parent: "/client",
      title: "客户端详情",
    },
  },
  {
    path: "/client/create",
    component: () => import("@/views/client/create/index.vue"),
    meta: {
      parent: "/client",
      title: "创建客户端",
    },
  },
  {
    path: "/user/detail",
    component: () => import("@/views/user/detail/index.vue"),
    meta: {
      parent: "/user/list",
      title: "用户详情",
    },
  },
  {
    path: "/user/create",
    component: () => import("@/views/user/create/index.vue"),
    meta: {
      parent: "/user/list",
      title: "创建用户",
    },
  },
  {
    path: "/user/group/detail",
    component: () => import("@/views/user/group/detail/index.vue"),
    meta: {
      parent: "/user/group",
      title: "用户组详情",
    },
  },
  {
    path: "/user/group/create",
    component: () => import("@/views/user/group/create/index.vue"),
    meta: {
      parent: "/user/group",
      title: "创建用户组",
    },
  },
  {
    path: "/role/detail",
    component: () => import("@/views/role/detail/index.vue"),
    meta: {
      parent: "/role",
      title: "角色详情",
    },
  },
  {
    path: "/role/create",
    component: () => import("@/views/role/create/index.vue"),
    meta: {
      parent: "/role",
      title: "创建角色",
    },
  },
  {
    path: "/permission/resource/detail",
    component: () => import("@/views/permission/resource/detail/index.vue"),
    meta: {
      parent: "/permission/resource",
      title: "资源详情",
    },
  },
  {
    path: "/permission/detail",
    component: () => import("@/views/permission/detail/index.vue"),
    meta: {
      parent: "/permission/resource",
      title: "权限详情",
    },
  },
  {
    path: "/permission/resource/create",
    component: () => import("@/views/permission/resource/create/index.vue"),
    meta: {
      parent: "/permission/resource",
      title: "创建资源",
    },
  },
  {
    path: "/permission/create",
    component: () => import("@/views/permission/create/index.vue"),
    meta: {
      parent: "/permission/resource",
      title: "创建权限",
    },
  },
  {
    path: "/permission/expression/detail",
    component: () => import("@/views/permission/expression/detail/index.vue"),
    meta: {
      parent: "/permission/expression",
      title: "限制条件详情",
    },
  },
  {
    path: "/permission/expression/create",
    component: () => import("@/views/permission/expression/create/index.vue"),
    meta: {
      parent: "/permission/expression",
      title: "创建限制条件",
    },
  },
  {
    path: "/permission/authorize",
    component: () => import("@/views/permission/authorize/index.vue"),
    meta: {
      parent: "/permission/resource",
      title: "授权",
    },
  },
  {
    path: "/resource/group/detail",
    component: () => import("@/views/resource/group/detail/index.vue"),
    meta: {
      parent: "/resource/group",
      title: "资源组详情",
    },
  },
  {
    path: "/resource/group/create",
    component: () => import("@/views/resource/group/create/index.vue"),
    meta: {
      parent: "/resource/group",
      title: "创建资源组",
    },
  },
  {
    path: "/user/attr/create",
    component: () => import("@/views/user/attr/create/index.vue"),
    meta: {
      parent: "/user/attr",
      title: "创建用户字段",
    },
  },
  {
    path: "/user/attr/detail",
    component: () => import("@/views/user/attr/detail/index.vue"),
    meta: {
      parent: "/user/attr",
      title: "用户字段详情",
    },
  },
  {
    path: "/tenant/detail",
    component: () => import("@/views/tenant/detail/index.vue"),
    meta: {
      parent: "/tenant",
      title: "租户详情",
    },
  },
  {
    path: "/tenant/create",
    component: () => import("@/views/tenant/create/index.vue"),
    meta: {
      parent: "/tenant",
      title: "创建租户",
    },
  },
  {
    path: "/dict/detail",
    component: () => import("@/views/dict/detail/index.vue"),
    meta: {
      parent: "/dict",
      title: "字典详情",
    },
  },
  {
    path: "/dict/create",
    component: () => import("@/views/dict/create/index.vue"),
    meta: {
      parent: "/dict",
      title: "创建字典",
    },
  },
  {
    path: "/dict/data/detail",
    component: () => import("@/views/dict/data/detail/index.vue"),
    meta: {
      parent: "/dict",
      title: "字典数据详情",
    },
  },
  {
    path: "/dict/data/create",
    component: () => import("@/views/dict/data/create/index.vue"),
    meta: {
      parent: "/dict",
      title: "创建字典数据",
    },
  },
  {
    path: "/system_setting/password/create",
    component: () => import("@/views/setting/password/create/index.vue"),
    meta: {
      parent: "/system_setting/password",
      title: "创建密码策略",
    },
  },
  {
    path: "/system_setting/password/detail",
    component: () => import("@/views/setting/password/detail/index.vue"),
    meta: {
      parent: "/system_setting/password",
      title: "密码策略详情",
    },
  },
  {
    path: "/identitySource/provider/detail",
    component: () => import("@/views/identitySource/provider/detail/index.vue"),
    meta: {
      parent: "/identitySource/provider/list",
      title: "身份源提供商详情",
    },
  },
  {
    path: "/identitySource/provider/create",
    component: () => import("@/views/identitySource/provider/create/index.vue"),
    meta: {
      parent: "/identitySource/provider/list",
      title: "创建身份源提供商",
    },
  },
  {
    path: "/identitySource/detail",
    component: () => import("@/views/identitySource/detail/index.vue"),
    meta: {
      parent: "/identitySource/list",
      title: "身份源详情",
    },
  },
  {
    path: "/identitySource/create",
    component: () => import("@/views/identitySource/create/index.vue"),
    meta: {
      parent: "/identitySource/list",
      title: "创建身份源",
    },
  },
  {
    path: "/permission/expression/template/create",
    component: () =>
      import("@/views/permission/expression/template/create/index.vue"),
    meta: {
      parent: "/permission/expression/template",
      title: "创建限制条件模版",
    },
  },
  {
    path: "/permission/expression/template/detail",
    component: () =>
      import("@/views/permission/expression/template/detail/index.vue"),
    meta: {
      parent: "/permission/expression/template",
      title: "限制条件模版详情",
    },
  },
  {
    path: "/chatbi/datasource/detail",
    component: () => import("@/views/chatbi/datasource/detail/index.vue"),
    meta: {
      parent: "/chatbi",
      title: "数据源详情",
    },
  },
  {
    path: "/chatbi/datasource/create",
    component: () => import("@/views/chatbi/datasource/create/index.vue"),
    meta: {
      parent: "/chatbi",
      title: "创建数据源",
    },
  },
];

export default pageRoutes;
