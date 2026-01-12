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
import { addChildDicts, deleteDict, getDictList, getRelatableDictDataList, getSelectableChildDictList, removeChildDicts, } from "@/api/dict";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";
/** 字典列表 */
var dictList = reactive([]);
var dictSerachKeyword = ref(null);
var dictPagination;
/**
 * 获取字典列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetDictList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getDictList({
        page: page,
        size: size,
        keyword: dictSerachKeyword.value,
        queryChildren: true,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            dictList.length = 0;
            dictList.push.apply(dictList, data.list);
            dictPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取字典列表");
    });
};
/** 可选子字典列表 */
var selectableChildDictList = reactive([]);
var relatableDictDataList = reactive([]);
var selectableChildDictModalVisible = ref(false);
/** 添加子字典表单 */
var addChildDictsForm = reactive({
    id: undefined,
    children: undefined,
    relatedDictDataIds: undefined,
});
var addChildDictsFormRef = ref();
var addChildDictsFormRules = {
    children: [
        { required: true, message: "请选择子字典", trigger: "blur" },
        {
            validator: function (value, cb) {
                var _a;
                if ((value === null || value === void 0 ? void 0 : value.length) !== ((_a = addChildDictsForm.relatedDictDataIds) === null || _a === void 0 ? void 0 : _a.length)) {
                    cb("子字典个数必须与所选关联字典数据个数一致");
                }
                else {
                    cb();
                }
            },
            trigger: "blur"
        },
    ],
    relatedDictDataIds: [
        { required: true, message: "请选择关联字典数据", trigger: "blur" },
        {
            validator: function (value, cb) {
                var _a;
                if ((value === null || value === void 0 ? void 0 : value.length) !== ((_a = addChildDictsForm.children) === null || _a === void 0 ? void 0 : _a.length)) {
                    cb("关联字典数据个数必须与所选子字典个数一致");
                }
                else {
                    cb();
                }
            },
            trigger: "blur"
        },
    ],
};
var handleAddChildDict = function (dictId) { return __awaiter(void 0, void 0, void 0, function () {
    var apiRes1, err_1, apiRes2, err_2;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, getSelectableChildDictList(dictId)];
            case 1:
                apiRes1 = _a.sent();
                handleApiSuccess(apiRes1, function (data) {
                    selectableChildDictList.length = 0;
                    selectableChildDictList.push.apply(selectableChildDictList, data);
                    if (selectableChildDictList.length === 0) {
                        Notification.warning("没有可添加的子字典");
                    }
                });
                return [3 /*break*/, 3];
            case 2:
                err_1 = _a.sent();
                handleApiError(err_1, "获取可选子字典列表");
                return [2 /*return*/];
            case 3:
                if ((selectableChildDictList === null || selectableChildDictList === void 0 ? void 0 : selectableChildDictList.length) === 0) {
                    return [2 /*return*/];
                }
                _a.label = 4;
            case 4:
                _a.trys.push([4, 6, , 7]);
                return [4 /*yield*/, getRelatableDictDataList(dictId)];
            case 5:
                apiRes2 = _a.sent();
                handleApiSuccess(apiRes2, function (data) {
                    relatableDictDataList.length = 0;
                    relatableDictDataList.push.apply(relatableDictDataList, data);
                    if ((relatableDictDataList === null || relatableDictDataList === void 0 ? void 0 : relatableDictDataList.length) === 0) {
                        Notification.warning("没有可关联的字典数据");
                    }
                });
                return [3 /*break*/, 7];
            case 6:
                err_2 = _a.sent();
                handleApiError(err_2, "获取可关联字典数据列表");
                return [2 /*return*/];
            case 7:
                if ((selectableChildDictList === null || selectableChildDictList === void 0 ? void 0 : selectableChildDictList.length) > 0 && (relatableDictDataList === null || relatableDictDataList === void 0 ? void 0 : relatableDictDataList.length) > 0) {
                    addChildDictsForm.id = dictId;
                    selectableChildDictModalVisible.value = true;
                }
                return [2 /*return*/];
        }
    });
}); };
/**
 * 添加子字典表单提交
 */
var handleAddChildDictsFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var errors, reqData, i;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, addChildDictsFormRef.value.validate()];
            case 1:
                errors = _a.sent();
                if (errors) {
                    selectableChildDictModalVisible.value = true;
                    return [2 /*return*/];
                }
                reqData = [];
                for (i = 0; i < addChildDictsForm.children.length; i++) {
                    reqData.push({
                        id: addChildDictsForm.id,
                        dataId: addChildDictsForm.relatedDictDataIds[i],
                        childId: addChildDictsForm.children[i],
                    });
                }
                addChildDicts(reqData).then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("添加成功");
                        handleCloseAddChildDictsModal();
                        handleGetDictList();
                    });
                });
                return [2 /*return*/];
        }
    });
}); };
/**
 * 关闭添加子字典弹框
 */
var handleCloseAddChildDictsModal = function () {
    addChildDictsForm.id = undefined;
    addChildDictsForm.children = undefined;
    addChildDictsForm.relatedDictDataIds = undefined;
    addChildDictsFormRef.value.resetFields();
    selectableChildDictModalVisible.value = false;
};
/**
 * 跳转字典详情
 *
 * @param dict 字典
 */
var handleToDictDetail = function (dict) {
    router.push({
        path: "/dict/detail",
        query: {
            id: dict.id,
            active_tab: "dict_info",
        },
    });
};
/**
 * 跳转创建字典
 *
 */
var handleToCreateDict = function () {
    router.push({
        path: "/dict/create",
    });
};
/**
 * 删除字典
 */
var handleDeleteDict = function (dict) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u5B57\u5178\u300C".concat(dict.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该字典数据关联的字典数据及用户字典，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteDict(dict.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetDictList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除字典");
            });
        },
    });
};
/**
 * 移除子字典
 */
var handleRemoveChildDict = function (dict) {
    Modal.warning({
        title: "\u786E\u5B9A\u79FB\u9664\u5B50\u5B57\u5178\u300C".concat(dict.name, "\u300D\u5417\uFF1F"),
        content: "",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeChildDicts({
                id: dict.parentId,
                childId: dict.id,
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("移除成功");
                    handleGetDictList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "移除子字典");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        dictPagination = usePagination("dictList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetDictList(page, size);
        });
        return {
            dictList: dictList,
            dictSerachKeyword: dictSerachKeyword,
            dictPagination: dictPagination,
            handleGetDictList: handleGetDictList,
            handleToDictDetail: handleToDictDetail,
            handleToCreateDict: handleToCreateDict,
            handleDeleteDict: handleDeleteDict,
            selectableChildDictModalVisible: selectableChildDictModalVisible,
            selectableChildDictList: selectableChildDictList,
            relatableDictDataList: relatableDictDataList,
            handleAddChildDict: handleAddChildDict,
            addChildDictsForm: addChildDictsForm,
            addChildDictsFormRef: addChildDictsFormRef,
            addChildDictsFormRules: addChildDictsFormRules,
            handleAddChildDictsFormSubmit: handleAddChildDictsFormSubmit,
            handleCloseAddChildDictsModal: handleCloseAddChildDictsModal,
            handleRemoveChildDict: handleRemoveChildDict,
        };
    },
});
