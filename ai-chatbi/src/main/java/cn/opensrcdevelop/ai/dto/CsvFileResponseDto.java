package cn.opensrcdevelop.ai.dto;

import java.io.Serializable;
import lombok.Data;

/**
 * CSV 文件响应 DTO
 */
@Data
public class CsvFileResponseDto implements Serializable {

    private static final long serialVersionUID = 1L;

    /** 表ID */
    private String tableId;

    /** 表名 */
    private String tableName;

    /** 注释 */
    private String remark;

    /** 是否启用 */
    private Boolean toUse;

    /** 补充信息 */
    private String additionalInfo;

    /** 字段数量 */
    private Integer fieldCount;

    /** 创建时间 */
    private String createdAt;
}
