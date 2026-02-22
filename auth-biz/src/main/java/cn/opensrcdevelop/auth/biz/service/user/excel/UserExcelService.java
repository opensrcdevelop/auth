package cn.opensrcdevelop.auth.biz.service.user.excel;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import java.util.List;
import org.springframework.web.multipart.MultipartFile;

public interface UserExcelService {

    /**
     * 生成用户导入模版
     *
     * @return Excel 文件字节数组
     */
    byte[] generateImportTemplate();

    /**
     * 同步导出用户数据
     *
     * @param filters
     *            筛选条件
     * @param exportAll
     *            是否导出全部
     * @param userIds
     *            用户ID列表（用于导出当前页）
     * @return Excel 文件字节数组
     */
    byte[] exportUsers(List<DataFilterDto> filters, boolean exportAll, List<String> userIds);

    /**
     * 同步导入用户数据
     *
     * @param file
     *            Excel 文件
     * @return 导入结果
     */
    ExcelImportResultDto importUsers(MultipartFile file);

    /**
     * 异步导出用户数据
     *
     * @param filters
     *            筛选条件
     * @param exportAll
     *            是否导出全部
     * @param userIds
     *            用户ID列表
     * @return 任务ID
     */
    String exportUsersAsync(List<DataFilterDto> filters, boolean exportAll, List<String> userIds);

    /**
     * 异步导入用户数据
     *
     * @param file
     *            Excel 文件
     * @return 任务ID
     */
    String importUsersAsync(MultipartFile file) throws java.io.IOException;
}
