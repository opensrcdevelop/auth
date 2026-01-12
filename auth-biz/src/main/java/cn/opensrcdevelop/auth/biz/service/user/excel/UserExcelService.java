package cn.opensrcdevelop.auth.biz.service.user.excel;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import org.springframework.web.multipart.MultipartFile;

public interface UserExcelService {

    /**
     * 生成用户导入模版
     *
     * @return Excel 文件字节数组
     */
    byte[] generateImportTemplate();

    /**
     * 导出用户数据
     *
     * @param filters
     *            筛选条件
     * @param exportAll
     *            是否导出全部
     * @return Excel 文件字节数组
     */
    byte[] exportUsers(java.util.List<DataFilterDto> filters, boolean exportAll);

    /**
     * 导入用户数据
     *
     * @param file
     *            Excel 文件
     * @return 导入结果
     */
    ExcelImportResultDto importUsers(MultipartFile file);
}
