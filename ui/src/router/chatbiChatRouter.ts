import {createRouter, createWebHistory} from "vue-router";

const routes = [
  {
    path: "/",
    redirect: "/chatbi/chat",
  },
  {
    path: "/chatbi/chat",
    component: () => import("@/views/chatbi/chat/index.vue"),
    meta: {
      title: "ChatBI 对话",
    },
  },
];

const router = createRouter({
  history: createWebHistory(),
  routes,
});

export default router;
