package cn.opensrcdevelop.ai.service.csv;

import cn.opensrcdevelop.ai.dto.CsvFileResponseDto;
import cn.opensrcdevelop.ai.dto.MultipartUploadCompleteRequestDto;
import cn.opensrcdevelop.ai.dto.MultipartUploadInitRequestDto;
import cn.opensrcdevelop.ai.dto.MultipartUploadInitResponseDto;
import java.util.List;

/**
 * CSV 文件服务接口
 */
public interface CsvFileService {

    /**
     * 初始化分片上传
     *
     * @param request
     *            初始化请求
     * @return 初始化响应（含上传ID）
     */
    MultipartUploadInitResponseDto initMultipartUpload(MultipartUploadInitRequestDto request);

    /**
     * 完成分片上传
     *
     * @param request
     *            完成请求
     * @return 任务ID
     */
    String completeMultipartUpload(MultipartUploadCompleteRequestDto request);

    /**
     * 生成分片上传预签名URL
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     * @param partNumber
     *            分片编号
     * @return 预签名URL
     */
    String generateUploadPartUrl(String key, String uploadId, int partNumber);

    /**
     * 获取数据源下的 CSV 文件列表
     *
     * @param dataSourceId
     *            数据源ID
     * @return CSV 文件列表
     */
    List<CsvFileResponseDto> listCsvFiles(String dataSourceId);

    /**
     * 取消分片上传
     *
     * @param key
     *            文件键
     * @param uploadId
     *            上传ID
     */
    void abortMultipartUpload(String key, String uploadId);

    /**
     * 删除 CSV 文件及关联的表结构
     *
     * @param dataSourceId
     *            数据源ID
     * @param fileName
     *            文件名（不含路径和 .csv 后缀）
     */
    void deleteCsv(String dataSourceId, String fileName);
}
