import { PERSISTENT_PAGE_INFO } from "@/util/constants";
import { onMounted, reactive } from "vue";
export var usePagination = function (key, getPageData) {
    var pagination = reactive({
        total: 0,
        current: 1,
        pageSize: 15,
        showPageSize: true,
        showTotal: true,
        pageSizeOptions: [15, 25, 50],
    });
    onMounted(function () {
        // 恢复分页数据
        var persistentPageInfo = localStorage.getItem(PERSISTENT_PAGE_INFO);
        if (persistentPageInfo) {
            var pageInfo = JSON.parse(persistentPageInfo);
            if (pageInfo[key]) {
                pagination.current = pageInfo[key].currentPage;
                pagination.pageSize = pageInfo[key].pageSize;
            }
        }
        // 获取分页数据
        getPageData({
            page: pagination.current,
            size: pagination.pageSize,
        });
    });
    // 页数变化处理
    var handlePageChange = function (page, filterFn) {
        if (filterFn === void 0) { filterFn = function () { return true; }; }
        if (filterFn()) {
            pagination.current = page;
            getPageData({
                page: page,
                size: pagination.pageSize,
            });
        }
    };
    // 分页大小变化处理
    var handlePageSizeChange = function (size, filterFn) {
        if (filterFn === void 0) { filterFn = function () { return true; }; }
        if (filterFn()) {
            pagination.pageSize = size;
            getPageData({
                page: 1,
                size: size,
            });
        }
    };
    // 保存分页数据
    var savePagination = function (currentPage, pageSize) {
        var _a;
        var persistentPageInfo = localStorage.getItem(PERSISTENT_PAGE_INFO);
        if (persistentPageInfo) {
            var pageInfo = JSON.parse(persistentPageInfo);
            pageInfo[key] = {
                currentPage: currentPage,
                pageSize: pageSize,
            };
            localStorage.setItem(PERSISTENT_PAGE_INFO, JSON.stringify(pageInfo));
        }
        else {
            var newPageInfo = (_a = {},
                _a[key] = {
                    currentPage: currentPage,
                    pageSize: pageSize,
                },
                _a);
            localStorage.setItem(PERSISTENT_PAGE_INFO, JSON.stringify(newPageInfo));
        }
    };
    // 更新分页数据
    var updatePagination = function (currentPage, total, pageSize) {
        pagination.current = currentPage;
        pagination.total = total;
        pagination.pageSize = pageSize;
        savePagination(pagination.current, pagination.pageSize);
    };
    return {
        pagination: pagination,
        handlePageChange: handlePageChange,
        handlePageSizeChange: handlePageSizeChange,
        updatePagination: updatePagination,
    };
};
