import {checkTenant} from "@/api/tenant";
import {AUTH_TOKENS, OAUTH_ISSUER, TENANT_CODE, TENANT_NAME,} from "@/util/constants";
import {getSubDomain} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {createRouter, createWebHistory, RouteRecordRaw} from "vue-router";
import menuRoutes from "./menuRoutes";
import pageRoutes from "./pageRoutes";

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
  if (visible && !visible()) {
    router.push("/404");
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
