package cn.opensrcdevelop.ai.service.csv;

import java.util.List;

/**
 * CSV 数据源存储服务接口
 * <p>
 * 提供 CSV 文件的 S3 存储能力，独立于 async-task 存储接口
 * </p>
 */
public interface CsvDatasourceStorageService {

    /**
     * 存储类型：S3 对象存储
     */
    String TYPE_S3 = "s3";

    /**
     * 保存文件
     *
     * @param data
     *            文件数据
     * @param key
     *            文件键
     */
    void store(byte[] data, String key);

    /**
     * 读取文件
     *
     * @param filePath
     *            文件路径
     * @return 文件数据
     */
    byte[] read(String filePath);

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
}
