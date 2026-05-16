package cn.opensrcdevelop.ai.service.csv;

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
     * @param data 文件数据
     * @param fileName 文件名
     * @return 文件存储路径
     */
    String store(byte[] data, String fileName);

    /**
     * 读取文件
     *
     * @param filePath 文件路径
     * @return 文件数据
     */
    byte[] read(String filePath);

    /**
     * 删除文件
     *
     * @param filePath 文件路径
     */
    void delete(String filePath);

    /**
     * 获取文件访问URL
     *
     * @param filePath 文件路径
     * @return 访问URL
     */
    String getUrl(String filePath);

    /**
     * 获取存储类型
     *
     * @return 存储类型
     */
    String getType();
}
