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
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createUserGroup } from "@/api/userGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import UserGroupConditions from "../components/UserGroupConditions.vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/**
 * 创建用户组表单
 */
var createUserGroupForm = reactive({
    name: "",
    code: "",
    type: "STATIC",
    desc: "",
    conditions: {
        filters: [
            {
                key: undefined,
                dataType: "STRING",
                value: undefined,
                filterType: undefined,
                extFlg: undefined,
            },
        ],
        conjunction: "AND",
        groups: [],
    },
});
var createUserGroupFormRef = ref();
var createUserGroupFormRules = {
    name: [{ required: true, message: "用户组名称未填写" }],
    code: [
        { required: true, message: "用户组标识未填写" },
        {
            validator: function (value, cb) {
                if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
                    cb("只允许包含英文字母、数字、下划线_、横线-");
                }
                else {
                    cb();
                }
            },
        },
    ],
    type: [{ required: true, message: "用户组类型未选择" }],
};
var userGroupConditionsRef = ref();
/**
 * 重置创建用户组表单
 */
var handleResetCreateUserGroupForm = function () {
    createUserGroupFormRef.value.resetFields();
};
/**
 * 提交创建用户组表单
 *
 * @param formData 创建用户组表单
 */
var handleCreateUserGroupFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateRes1, validateRes2;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, createUserGroupFormRef.value.validate()];
            case 1:
                validateRes1 = _a.sent();
                validateRes2 = true;
                if (!userGroupConditionsRef.value) return [3 /*break*/, 3];
                return [4 /*yield*/, userGroupConditionsRef.value.validate()];
            case 2:
                validateRes2 = _a.sent();
                _a.label = 3;
            case 3:
                if (validateRes1 !== undefined || !validateRes2) {
                    return [2 /*return*/];
                }
                if (createUserGroupForm.type === "STATIC") {
                    createUserGroupForm.conditions = undefined;
                }
                createUserGroup(createUserGroupForm)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("创建成功");
                        handleResetCreateUserGroupForm();
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "创建用户组");
                });
                return [2 /*return*/];
        }
    });
}); };
var handleUserGroupTypeChange = function (val) {
    if (val === "DYNAMIC") {
        createUserGroupForm.conditions = {
            filters: [
                {
                    key: undefined,
                    dataType: "STRING",
                    value: undefined,
                    filterType: undefined,
                    extFlg: undefined,
                },
            ],
            conjunction: "AND",
            groups: [],
        };
    }
};
export default defineComponent({
    components: {
        UserGroupConditions: UserGroupConditions,
    },
    setup: function () {
        return {
            handleBack: handleBack,
            createUserGroupForm: createUserGroupForm,
            createUserGroupFormRef: createUserGroupFormRef,
            createUserGroupFormRules: createUserGroupFormRules,
            handleResetCreateUserGroupForm: handleResetCreateUserGroupForm,
            handleCreateUserGroupFormSubmit: handleCreateUserGroupFormSubmit,
            userGroupConditionsRef: userGroupConditionsRef,
            handleUserGroupTypeChange: handleUserGroupTypeChange,
        };
    },
});
