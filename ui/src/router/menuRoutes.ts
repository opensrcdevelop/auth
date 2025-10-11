import {isTenant} from "@/util/tool";
import {RouteRecordRaw} from "vue-router";

const menuRoutes: RouteRecordRaw[] = [
  {
    path: "/client",
    component: () => import("@/views/client/index.vue"),
    meta: {
      title: "客户端",
      icon: "icon-app",
      visible: () => true,
    },
  },
  {
    path: "/user",
    meta: {
      title: "用户管理",
      icon: "icon-user",
      visible: () => true,
    },
    children: [
      {
        path: "/user/list",
        component: () => import("@/views/user/index.vue"),
        meta: {
          title: "用户列表",
          visible: () => true,
        },
      },
      {
        path: "/user/group",
        component: () => import("@/views/user/group/index.vue"),
        meta: {
          title: "用户组管理",
          visible: () => true,
        },
      },
      {
        path: "/user/attr",
        component: () => import("@/views/user/attr/index.vue"),
        meta: {
          title: "字段管理",
          visible: () => true,
        },
      },
    ],
  },
  {
    path: "/resource",
    meta: {
      title: "资源管理",
      icon: "icon-resource",
      visible: () => true,
    },
    children: [
      {
        path: "/resource/group",
        component: () => import("@/views/resource/group/index.vue"),
        meta: {
          title: "资源组管理",
          visible: () => true,
        },
      },
    ],
  },
  {
    path: "/permission",
    meta: {
      title: "权限管理",
      icon: "icon-permission",
      visible: () => true,
    },
    children: [
      {
        path: "/permission/resource",
        component: () => import("@/views/permission/resource/index.vue"),
        meta: {
          title: "资源权限",
          visible: () => true,
        },
      },
      {
        path: "/role",
        component: () => import("@/views/role/index.vue"),
        meta: {
          title: "角色管理",
          visible: () => true,
        },
      },
      {
        path: "/permission/expression",
        component: () => import("@/views/permission/expression/index.vue"),
        meta: {
          title: "限制条件",
          visible: () => true,
        },
      },
      {
        path: "/permission/expression/template",
        component: () =>
          import("@/views/permission/expression/template/index.vue"),
        meta: {
          title: "限制条件模板",
          visible: () => true,
        },
      },
    ],
  },
  {
    path: "/identitySource",
    meta: {
      title: "身份源管理",
      icon: "icon-identitySource",
      visible: () => true,
    },
    children: [
      {
        path: "/identitySource/provider/list",
        component: () => import("@/views/identitySource/provider/index.vue"),
        meta: {
          title: "提供商管理",
          visible: () => true,
        },
      },
      {
        path: "/identitySource/list",
        component: () => import("@/views/identitySource/index.vue"),
        meta: {
          title: "身份源列表",
          visible: () => true,
        },
      },
    ],
  },
  {
    path: "/tenant",
    component: () => import("@/views/tenant/index.vue"),
    meta: {
      title: "多租户",
      icon: "icon-tenant",
      visible: () => !isTenant(),
    },
  },
  {
    path: "/dict",
    component: () => import("@/views/dict/index.vue"),
    meta: {
      title: "数据字典",
      icon: "icon-dict",
      visible: () => true,
    },
  },
  {
    path: "/chatbi",
    component: () => import("@/views/chatbi/index.vue"),
    meta: {
      title: "ChatBI 问数",
      icon: "icon-chat",
      visible: () => true,
    },
  },
  {
    path: "/audit/logs",
    meta: {
      title: "审计日志",
      icon: "icon-auditLog",
      visible: () => true,
    },
    children: [
      {
        path: "/audit/logs/user",
        component: () => import("@/views/audit/userOperation/index.vue"),
        meta: {
          title: "用户操作日志",
          visible: () => true,
        },
      },
      {
        path: "/audit/logs/sys",
        component: () => import("@/views/audit/sysOperation/index.vue"),
        meta: {
          title: "系统操作日志",
          visible: () => true,
        },
      },
    ],
  },
  {
    path: "/system_setting",
    meta: {
      title: "系统设置",
      icon: "icon-system",
      visible: () => true,
    },
    children: [
      {
        path: "/system_setting/message",
        component: () => import("@/views/setting/message/index.vue"),
        meta: {
          title: "消息设置",
          visible: () => true,
        },
      },
      {
        path: "/system_setting/password",
        component: () => import("@/views/setting/password/index.vue"),
        meta: {
          title: "密码安全",
          visible: () => true,
        },
      },
      {
        path: "/system_setting/jwt",
        component: () => import("@/views/setting/jwt/index.vue"),
        meta: {
          title: "JWT 设置",
          visible: () => true,
        },
      },
    ],
  },
];

export default menuRoutes;
