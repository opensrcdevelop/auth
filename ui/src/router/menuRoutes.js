import { isTenant } from "@/util/tool";
var menuRoutes = [
    {
        path: "/client",
        component: function () { return import("@/views/client/index.vue"); },
        meta: {
            title: "客户端",
            icon: "icon-app",
            visible: function () { return true; },
        },
    },
    {
        path: "/user",
        meta: {
            title: "用户管理",
            icon: "icon-user",
            visible: function () { return true; },
        },
        children: [
            {
                path: "/user/list",
                component: function () { return import("@/views/user/index.vue"); },
                meta: {
                    title: "用户列表",
                    visible: function () { return true; },
                },
            },
            {
                path: "/user/group",
                component: function () { return import("@/views/user/group/index.vue"); },
                meta: {
                    title: "用户组管理",
                    visible: function () { return true; },
                },
            },
            {
                path: "/user/attr",
                component: function () { return import("@/views/user/attr/index.vue"); },
                meta: {
                    title: "字段管理",
                    visible: function () { return true; },
                },
            },
        ],
    },
    {
        path: "/resource",
        meta: {
            title: "资源管理",
            icon: "icon-resource",
            visible: function () { return true; },
        },
        children: [
            {
                path: "/resource/group",
                component: function () { return import("@/views/resource/group/index.vue"); },
                meta: {
                    title: "资源组管理",
                    visible: function () { return true; },
                },
            },
        ],
    },
    {
        path: "/permission",
        meta: {
            title: "权限管理",
            icon: "icon-permission",
            visible: function () { return true; },
        },
        children: [
            {
                path: "/permission/resource",
                component: function () { return import("@/views/permission/resource/index.vue"); },
                meta: {
                    title: "资源权限",
                    visible: function () { return true; },
                },
            },
            {
                path: "/role",
                component: function () { return import("@/views/role/index.vue"); },
                meta: {
                    title: "角色管理",
                    visible: function () { return true; },
                },
            },
            {
                path: "/permission/expression",
                component: function () { return import("@/views/permission/expression/index.vue"); },
                meta: {
                    title: "限制条件",
                    visible: function () { return true; },
                },
            },
            {
                path: "/permission/expression/template",
                component: function () {
                    return import("@/views/permission/expression/template/index.vue");
                },
                meta: {
                    title: "限制条件模板",
                    visible: function () { return true; },
                },
            },
        ],
    },
    {
        path: "/identitySource",
        meta: {
            title: "身份源管理",
            icon: "icon-identitySource",
            visible: function () { return true; },
        },
        children: [
            {
                path: "/identitySource/provider/list",
                component: function () { return import("@/views/identitySource/provider/index.vue"); },
                meta: {
                    title: "提供商管理",
                    visible: function () { return true; },
                },
            },
            {
                path: "/identitySource/list",
                component: function () { return import("@/views/identitySource/index.vue"); },
                meta: {
                    title: "身份源列表",
                    visible: function () { return true; },
                },
            },
        ],
    },
    {
        path: "/tenant",
        component: function () { return import("@/views/tenant/index.vue"); },
        meta: {
            title: "多租户",
            icon: "icon-tenant",
            visible: function () { return !isTenant(); },
        },
    },
    {
        path: "/dict",
        component: function () { return import("@/views/dict/index.vue"); },
        meta: {
            title: "数据字典",
            icon: "icon-dict",
            visible: function () { return true; },
        },
    },
    {
        path: "/chatbi",
        component: function () { return import("@/views/chatbi/index.vue"); },
        meta: {
            title: "ChatBI 问数",
            icon: "icon-chat",
            visible: function () { return true; },
        },
    },
    {
        path: "/audit/logs",
        meta: {
            title: "审计日志",
            icon: "icon-auditLog",
            visible: function () { return true; },
        },
        children: [
            {
                path: "/audit/logs/user",
                component: function () { return import("@/views/audit/userOperation/index.vue"); },
                meta: {
                    title: "用户操作日志",
                    visible: function () { return true; },
                },
            },
            {
                path: "/audit/logs/sys",
                component: function () { return import("@/views/audit/sysOperation/index.vue"); },
                meta: {
                    title: "系统操作日志",
                    visible: function () { return true; },
                },
            },
        ],
    },
    {
        path: "/system_setting",
        meta: {
            title: "系统设置",
            icon: "icon-system",
            visible: function () { return true; },
        },
        children: [
            {
                path: "/system_setting/message",
                component: function () { return import("@/views/setting/message/index.vue"); },
                meta: {
                    title: "消息设置",
                    visible: function () { return true; },
                },
            },
            {
                path: "/system_setting/password",
                component: function () { return import("@/views/setting/password/index.vue"); },
                meta: {
                    title: "密码安全",
                    visible: function () { return true; },
                },
            },
            {
                path: "/system_setting/jwt",
                component: function () { return import("@/views/setting/jwt/index.vue"); },
                meta: {
                    title: "JWT 设置",
                    visible: function () { return true; },
                },
            },
        ],
    },
];
export default menuRoutes;
