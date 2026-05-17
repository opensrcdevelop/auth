package cn.opensrcdevelop.ai.service.csv;

import cn.opensrcdevelop.ai.service.csv.impl.CsvDatasourceStorageServiceImpl;
import java.util.List;

/**
 * CSV 数据源存储服务接口
 * <p>
 * 提供 CSV 文件的 S3 存储能力，独立于 async-task 存储接口
 * </p>
 */
public interface CsvDatasourceStorageService {

    /**
     * 删除文件
     *
     * @param filePath
     *            文件路径
     */
    void delete(String filePath);

    /**
     * 获取文件访问URL
     *
     * @param filePath
     *            文件路径
     * @return 访问URL
     */
    String getUrl(String filePath);

    /**
     * 列出指定前缀的所有文件路径
     *
     * @param prefix
     *            文件前缀
     * @return 文件路径列表
     */
    List<String> list(String prefix);

    /**
     * 初始化分段上传
     *
     * @param key
     *            文件键
     * @return 上传ID
     */
    String initiateMultipartUpload(String key);

    /**
     * 生成分片上传预签名URL
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     * @param partNumber
     *            分片编号（从1开始）
     * @return 预签名URL
     */
    String generateUploadPartUrl(String key, String uploadId, int partNumber);

    /**
     * 完成分段上传
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     * @param parts
     *            已上传的分片信息
     */
    void completeMultipartUpload(String key, String uploadId, List<CsvDatasourceStorageServiceImpl.UploadedPart> parts);

    /**
     * 取消分段上传
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     */
    void abortMultipartUpload(String key, String uploadId);
}
