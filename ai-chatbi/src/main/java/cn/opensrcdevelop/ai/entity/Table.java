package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_table")
@EntityName("数据源表")
public class Table extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -3494423245247462542L;

    /** 数据源ID */
    @PropertyName("数据源ID")
    private String dataSourceId;

    /** 表ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("表ID")
    private String tableId;

    /** 表名 */
    @PropertyName("表名")
    private String tableName;

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
