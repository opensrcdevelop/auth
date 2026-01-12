var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
import { checkTenant } from "@/api/tenant";
import { AUTH_TOKENS, OAUTH_ISSUER, TENANT_CODE, TENANT_NAME, } from "@/util/constants";
import { getSubDomain } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { createRouter, createWebHistory } from "vue-router";
import menuRoutes from "./menuRoutes";
import pageRoutes from "./pageRoutes";
export var routes = [
    {
        path: "/",
        component: function () { return import("@/layout/index.vue"); },
        redirect: "/client",
        children: __spreadArray(__spreadArray([], menuRoutes, true), pageRoutes, true),
    },
    {
        path: "/login",
        component: function () { return import("@/views/login/index.vue"); },
        meta: {
            title: "登录",
        },
    },
    {
        path: "/oauth2/redirect",
        component: function () { return import("@/views/oauth2/index.vue"); },
    },
    {
        path: "/login/changePwd",
        component: function () { return import("@/views/login/changePwd/index.vue"); },
        meta: {
            title: "修改密码",
        },
    },
    {
        path: "/user/home",
        component: function () { return import("@/views/user/home/index.vue"); },
        meta: {
            title: "个人中心",
        },
    },
    {
        path: "/403",
        component: function () { return import("@/views/403/index.vue"); },
    },
    {
        path: "/404",
        component: function () { return import("@/views/404/index.vue"); },
    },
    {
        path: "/:pathMatch(.*)*",
        redirect: "/404",
    },
];
var router = createRouter({
    history: createWebHistory(import.meta.env.VITE_UI_BASE_PATH),
    routes: routes,
});
/**
 * 前置路由拦截
 */
router.beforeEach(function (to, from, next) {
    var visible = to.meta.visible;
    if (visible && !visible()) {
        router.push("/404");
    }
    window.document.title = to.meta.title
        ? "Auth Server - ".concat(to.meta.title)
        : "Auth Server";
    handleCheckTenant(to).then(function () {
        if (!localStorage.getItem(AUTH_TOKENS)) {
            if (to.path === "/oauth2/redirect" ||
                to.path === "/login" ||
                to.path === "/login/changePwd" ||
                to.path === "/404" ||
                to.path === "/403") {
                next();
            }
            else {
                router.push("/oauth2/redirect");
            }
        }
        else {
            next();
        }
    });
});
/**
 * 检查租户是否存在
 */
function handleCheckTenant(to) {
    return __awaiter(this, void 0, void 0, function () {
        var tenantCode, checkRes, data, error_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    if (to.path === "/404" || to.path === "/403") {
                        return [2 /*return*/];
                    }
                    tenantCode = getSubDomain();
                    if (!tenantCode) return [3 /*break*/, 5];
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 3, , 4]);
                    return [4 /*yield*/, checkTenant(tenantCode)];
                case 2:
                    checkRes = _a.sent();
                    data = checkRes.data;
                    if (data.exists) {
                        localStorage.setItem(OAUTH_ISSUER, data.issuer);
                        localStorage.setItem(TENANT_CODE, tenantCode);
                        localStorage.setItem(TENANT_NAME, data.tenantName);
                    }
                    else {
                        // 租户不存在
                        localStorage.removeItem(OAUTH_ISSUER);
                        localStorage.removeItem(TENANT_CODE);
                        localStorage.removeItem(TENANT_NAME);
                        router.push({
                            path: "/404",
                        });
                    }
                    return [3 /*break*/, 4];
                case 3:
                    error_1 = _a.sent();
                    Notification.error("检查租户标识错误");
                    return [3 /*break*/, 4];
                case 4: return [3 /*break*/, 6];
                case 5:
                    localStorage.removeItem(OAUTH_ISSUER);
                    localStorage.removeItem(TENANT_CODE);
                    localStorage.removeItem(TENANT_NAME);
                    _a.label = 6;
                case 6: return [2 /*return*/];
            }
        });
    });
}
export default router;
