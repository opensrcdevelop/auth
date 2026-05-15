package cn.opensrcdevelop.ai.service.csv;

import cn.opensrcdevelop.ai.dto.CsvFileResponseDto;
import java.util.List;
import org.springframework.web.multipart.MultipartFile;

/**
 * CSV 文件服务接口
 */
public interface CsvFileService {

    /**
     * 上传 CSV 文件到 S3 并提交异步解析任务
     *
     * @param file
     *            上传的文件
     * @param dataSourceId
     *            数据源ID
     * @return 任务ID
     */
    String uploadCsv(MultipartFile file, String dataSourceId);

    /**
     * 获取数据源下的 CSV 文件列表
     *
     * @param dataSourceId
     *            数据源ID
     * @return CSV 文件列表
     */
    List<CsvFileResponseDto> listCsvFiles(String dataSourceId);

    /**
     * 删除 CSV 文件及关联的表结构
     *
     * @param tableId
     *            表ID
     */
    void deleteCsv(String tableId);

    /**
     * 更新 CSV 文件（替换）并重新解析表结构
     *
     * @param tableId
     *            表ID
     * @param file
     *            上传的新文件
     * @return 任务ID
     */
    String updateCsv(String tableId, MultipartFile file);
}
