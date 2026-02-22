package cn.opensrcdevelop.auth.biz.service.asynctask.storage;

/**
 * 存储服务接口
 */
public interface StorageService {

    /**
     * 存储类型：本地存储
     */
    String TYPE_LOCAL = "local";

    /**
     * 存储类型：S3 对象存储
     */
    String TYPE_S3 = "s3";

    /**
     * 保存文件
     *
     * @param data
     *            文件数据
     * @param fileName
     *            文件名
     * @return 文件存储路径
     */
    String store(byte[] data, String fileName);

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
     * 获取存储类型
     *
     * @return 存储类型
     */
    String getType();
}
