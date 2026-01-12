var pageRoutes = [
    {
        path: "/client/detail",
        component: function () { return import("@/views/client/detail/index.vue"); },
        meta: {
            parent: "/client",
            title: "客户端详情",
        },
    },
    {
        path: "/client/create",
        component: function () { return import("@/views/client/create/index.vue"); },
        meta: {
            parent: "/client",
            title: "创建客户端",
        },
    },
    {
        path: "/user/detail",
        component: function () { return import("@/views/user/detail/index.vue"); },
        meta: {
            parent: "/user/list",
            title: "用户详情",
        },
    },
    {
        path: "/user/create",
        component: function () { return import("@/views/user/create/index.vue"); },
        meta: {
            parent: "/user/list",
            title: "创建用户",
        },
    },
    {
        path: "/user/group/detail",
        component: function () { return import("@/views/user/group/detail/index.vue"); },
        meta: {
            parent: "/user/group",
            title: "用户组详情",
        },
    },
    {
        path: "/user/group/create",
        component: function () { return import("@/views/user/group/create/index.vue"); },
        meta: {
            parent: "/user/group",
            title: "创建用户组",
        },
    },
    {
        path: "/role/detail",
        component: function () { return import("@/views/role/detail/index.vue"); },
        meta: {
            parent: "/role",
            title: "角色详情",
        },
    },
    {
        path: "/role/create",
        component: function () { return import("@/views/role/create/index.vue"); },
        meta: {
            parent: "/role",
            title: "创建角色",
        },
    },
    {
        path: "/permission/resource/detail",
        component: function () { return import("@/views/permission/resource/detail/index.vue"); },
        meta: {
            parent: "/permission/resource",
            title: "资源详情",
        },
    },
    {
        path: "/permission/detail",
        component: function () { return import("@/views/permission/detail/index.vue"); },
        meta: {
            parent: "/permission/resource",
            title: "权限详情",
        },
    },
    {
        path: "/permission/resource/create",
        component: function () { return import("@/views/permission/resource/create/index.vue"); },
        meta: {
            parent: "/permission/resource",
            title: "创建资源",
        },
    },
    {
        path: "/permission/create",
        component: function () { return import("@/views/permission/create/index.vue"); },
        meta: {
            parent: "/permission/resource",
            title: "创建权限",
        },
    },
    {
        path: "/permission/expression/detail",
        component: function () { return import("@/views/permission/expression/detail/index.vue"); },
        meta: {
            parent: "/permission/expression",
            title: "限制条件详情",
        },
    },
    {
        path: "/permission/expression/create",
        component: function () { return import("@/views/permission/expression/create/index.vue"); },
        meta: {
            parent: "/permission/expression",
            title: "创建限制条件",
        },
    },
    {
        path: "/permission/authorize",
        component: function () { return import("@/views/permission/authorize/index.vue"); },
        meta: {
            parent: "/permission/resource",
            title: "授权",
        },
    },
    {
        path: "/resource/group/detail",
        component: function () { return import("@/views/resource/group/detail/index.vue"); },
        meta: {
            parent: "/resource/group",
            title: "资源组详情",
        },
    },
    {
        path: "/resource/group/create",
        component: function () { return import("@/views/resource/group/create/index.vue"); },
        meta: {
            parent: "/resource/group",
            title: "创建资源组",
        },
    },
    {
        path: "/user/attr/create",
        component: function () { return import("@/views/user/attr/create/index.vue"); },
        meta: {
            parent: "/user/attr",
            title: "创建用户字段",
        },
    },
    {
        path: "/user/attr/detail",
        component: function () { return import("@/views/user/attr/detail/index.vue"); },
        meta: {
            parent: "/user/attr",
            title: "用户字段详情",
        },
    },
    {
        path: "/tenant/detail",
        component: function () { return import("@/views/tenant/detail/index.vue"); },
        meta: {
            parent: "/tenant",
            title: "租户详情",
        },
    },
    {
        path: "/tenant/create",
        component: function () { return import("@/views/tenant/create/index.vue"); },
        meta: {
            parent: "/tenant",
            title: "创建租户",
        },
    },
    {
        path: "/dict/detail",
        component: function () { return import("@/views/dict/detail/index.vue"); },
        meta: {
            parent: "/dict",
            title: "字典详情",
        },
    },
    {
        path: "/dict/create",
        component: function () { return import("@/views/dict/create/index.vue"); },
        meta: {
            parent: "/dict",
            title: "创建字典",
        },
    },
    {
        path: "/dict/data/detail",
        component: function () { return import("@/views/dict/data/detail/index.vue"); },
        meta: {
            parent: "/dict",
            title: "字典数据详情",
        },
    },
    {
        path: "/dict/data/create",
        component: function () { return import("@/views/dict/data/create/index.vue"); },
        meta: {
            parent: "/dict",
            title: "创建字典数据",
        },
    },
    {
        path: "/system_setting/password/create",
        component: function () { return import("@/views/setting/password/create/index.vue"); },
        meta: {
            parent: "/system_setting/password",
            title: "创建密码策略",
        },
    },
    {
        path: "/system_setting/password/detail",
        component: function () { return import("@/views/setting/password/detail/index.vue"); },
        meta: {
            parent: "/system_setting/password",
            title: "密码策略详情",
        },
    },
    {
        path: "/identitySource/provider/detail",
        component: function () { return import("@/views/identitySource/provider/detail/index.vue"); },
        meta: {
            parent: "/identitySource/provider/list",
            title: "身份源提供商详情",
        },
    },
    {
        path: "/identitySource/provider/create",
        component: function () { return import("@/views/identitySource/provider/create/index.vue"); },
        meta: {
            parent: "/identitySource/provider/list",
            title: "创建身份源提供商",
        },
    },
    {
        path: "/identitySource/detail",
        component: function () { return import("@/views/identitySource/detail/index.vue"); },
        meta: {
            parent: "/identitySource/list",
            title: "身份源详情",
        },
    },
    {
        path: "/identitySource/create",
        component: function () { return import("@/views/identitySource/create/index.vue"); },
        meta: {
            parent: "/identitySource/list",
            title: "创建身份源",
        },
    },
    {
        path: "/permission/expression/template/create",
        component: function () {
            return import("@/views/permission/expression/template/create/index.vue");
        },
        meta: {
            parent: "/permission/expression/template",
            title: "创建限制条件模版",
        },
    },
    {
        path: "/permission/expression/template/detail",
        component: function () {
            return import("@/views/permission/expression/template/detail/index.vue");
        },
        meta: {
            parent: "/permission/expression/template",
            title: "限制条件模版详情",
        },
    },
    {
        path: "/chatbi/datasource/detail",
        component: function () { return import("@/views/chatbi/datasource/detail/index.vue"); },
        meta: {
            parent: "/chatbi",
            title: "数据源详情",
        },
    },
    {
        path: "/chatbi/datasource/create",
        component: function () { return import("@/views/chatbi/datasource/create/index.vue"); },
        meta: {
            parent: "/chatbi",
            title: "创建数据源",
        },
    },
    {
        path: "/chatbi/llm/detail",
        component: function () { return import("@/views/chatbi/llm/detail/index.vue"); },
        meta: {
            parent: "/chatbi",
            title: "模型提供商详情",
        },
    },
    {
        path: "/chatbi/llm/create",
        component: function () { return import("@/views/chatbi/llm/create/index.vue"); },
        meta: {
            parent: "/chatbi",
            title: "创建模型提供商",
        },
    },
];
export default pageRoutes;
