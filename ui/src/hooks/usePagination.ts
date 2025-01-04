import { PERSISTENT_PAGE_INFO } from "@/util/constants"
import { onMounted, reactive } from "vue"

export const usePagination = (key: string, getPageData: Function) => {

    const pagination = reactive({
        total: 0,
        current: 1,
        pageSize: 15,
        showPageSize: true,
        showTotal: true,
        pageSizeOptions: [15, 25, 50],
    })

    onMounted(() => {
        // 恢复分页数据
        const persistentPageInfo = localStorage.getItem(PERSISTENT_PAGE_INFO);
        if (persistentPageInfo) {
            const pageInfo = JSON.parse(persistentPageInfo);
            if (pageInfo[key]) {
                pagination.current = pageInfo[key].currentPage;
                pagination.pageSize = pageInfo[key].pageSize;
            }
        }

        // 获取分页数据
        getPageData({
            page: pagination.current,
            size: pagination.pageSize
        })
    })

    // 页数变化处理
    const handlePageChange = (page: number) => {
        pagination.current = page;
        getPageData({
            page,
            size: pagination.pageSize
        });
    }

    // 分页大小变化处理
    const handlePageSizeChange = (size: number) => {
        pagination.pageSize = size;
        getPageData({
            page: 1,
            size
        });
    }

    // 保存分页数据
    const savePagination = (currentPage, pageSize) => {
        const persistentPageInfo = localStorage.getItem(PERSISTENT_PAGE_INFO);
        if (persistentPageInfo) {
            const pageInfo = JSON.parse(persistentPageInfo);
            pageInfo[key] = {
                currentPage,
                pageSize
            }
            localStorage.setItem(PERSISTENT_PAGE_INFO, JSON.stringify(pageInfo));
        } else {
            const newPageInfo = {
                [key]: {
                    currentPage,
                    pageSize
                }
            }
            localStorage.setItem(PERSISTENT_PAGE_INFO, JSON.stringify(newPageInfo));
        }
    }

    // 更新分页数据
    const updatePagination = (currentPage, total, pageSize) => {
        pagination.current = currentPage;
        pagination.total = total;
        pagination.pageSize = pageSize;
        savePagination(pagination.current, pagination.pageSize);
    }

    return {
        pagination,
        handlePageChange,
        handlePageSizeChange,
        updatePagination
    }
}