package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_table_field")
public class TableField extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6488902898717594612L;

    /** 表ID */
    @TableId(type = IdType.INPUT)
    private String tableId;

    /** 字段ID */
    private String fieldId;

    /** 字段名 */
    private String fieldName;

    /** 字段类型 */
    private String fieldType;

    /** 注释 */
    private String remark;

    /** 是否使用 */
    private Boolean toUse;

    /** 补充信息 */
    private String additionalInfo;
}
