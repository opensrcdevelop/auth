package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
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
@EntityName("数据源表字段")
public class TableField extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6488902898717594612L;

    /** 字段ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("字段ID")
    private String fieldId;

    /** 表ID */
    @PropertyName("数据源表ID")
    private String tableId;

    /** 字段名 */
    @PropertyName("字段名")
    private String fieldName;

    /** 字段类型 */
    @PropertyName("字段类型")
    private String fieldType;

    /** 注释 */
    @PropertyName("注释")
    private String remark;

    /** 是否使用 */
    @PropertyName("是否使用")
    private Boolean toUse;

    /** 补充信息 */
    @PropertyName("补充信息")
    private String additionalInfo;
}
