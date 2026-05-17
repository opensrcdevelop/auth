package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * CSV 文件响应 DTO
 */
@Data
public class CsvFileResponseDto implements Serializable {

    @Serial
    private static final long serialVersionUID = -1612928596027237227L;

    /** 表ID */
    private String tableId;

    /** 文件名 */
    private String fileName;

    /** 上传时间 */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime uploadTime;

    /** 字段数量 */
    private Integer fieldCount;
}
