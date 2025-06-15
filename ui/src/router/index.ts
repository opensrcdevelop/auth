import {checkTenant} from "@/api/tenant";
import {AUTH_TOKENS, OAUTH_ISSUER, TENANT_CODE, TENANT_NAME,} from "@/util/constants";
import {getSubDomain, isTenant} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {createRouter, createWebHistory, RouteRecordRaw} from "vue-router";

/** 菜单路由 */
export const menuRoutes: RouteRecordRaw[] = [
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

/** 页面路由 */
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
    path: "/permission/expression/debug",
    component: () => import("@/views/permission/expression/debug/index.vue"),
    meta: {
      parent: "/permission/expression",
      title: "调试限制条件",
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
];

export const routes: RouteRecordRaw[] = [
  {
    path: "/",
    component: () => import("@/layout/index.vue"),
    redirect: "/client",
    children: [...menuRoutes, ...pageRoutes],
  },
  {
    path: "/login",
    component: () => import("@/views/login/index.vue"),
    meta: {
      title: "登录",
    },
  },
  {
    path: "/oauth2/redirect",
    component: () => import("@/views/oauth2/index.vue"),
  },
  {
    path: "/login/changePwd",
    component: () => import("@/views/login/changePwd/index.vue"),
    meta: {
      title: "修改密码",
    },
  },
  {
    path: "/user/home",
    component: () => import("@/views/user/home/index.vue"),
    meta: {
      title: "个人中心",
    },
  },
  {
    path: "/403",
    component: () => import("@/views/403/index.vue"),
  },
  {
    path: "/404",
    component: () => import("@/views/404/index.vue"),
  },
  {
    path: "/:pathMatch(.*)*",
    redirect: "/404",
  },
];

const router = createRouter({
  history: createWebHistory(import.meta.env.VITE_UI_BASE_PATH),
  routes: routes,
});

/**
 * 前置路由拦截
 */
router.beforeEach((to, from, next) => {
  const visible = to.meta.visible as Function;
  console.log(to.meta)
  if (visible && !visible()) {
    router.push("/404")
  }

  window.document.title = to.meta.title
    ? (`Auth Server - ${to.meta.title}` as string)
    : "Auth Server";
  handleCheckTenant(to).then(() => {
    if (!localStorage.getItem(AUTH_TOKENS)) {
      if (
        to.path === "/oauth2/redirect" ||
        to.path === "/login" ||
        to.path === "/login/changePwd" ||
        to.path === "/404" ||
        to.path === "/403"
      ) {
        next();
      } else {
        router.push("/oauth2/redirect");
      }
    } else {
      next();
    }
  });
});

/**
 * 检查租户是否存在
 */
async function handleCheckTenant(to: any) {
  if (to.path === "/404" || to.path === "/403") {
    return;
  }

  const tenantCode = getSubDomain();
  if (tenantCode) {
    // 检查租户标识
    try {
      const checkRes = await checkTenant(tenantCode);
      const data = checkRes.data as any;
      if (data.exists) {
        localStorage.setItem(OAUTH_ISSUER, data.issuer);
        localStorage.setItem(TENANT_CODE, tenantCode);
        localStorage.setItem(TENANT_NAME, data.tenantName);
      } else {
        // 租户不存在
        localStorage.removeItem(OAUTH_ISSUER);
        localStorage.removeItem(TENANT_CODE);
        localStorage.removeItem(TENANT_NAME);
        router.push({
          path: "/404",
        });
      }
    } catch (error) {
      Notification.error("检查租户标识错误");
    }
  } else {
    localStorage.removeItem(OAUTH_ISSUER);
    localStorage.removeItem(TENANT_CODE);
    localStorage.removeItem(TENANT_NAME);
  }
}

export default router;
