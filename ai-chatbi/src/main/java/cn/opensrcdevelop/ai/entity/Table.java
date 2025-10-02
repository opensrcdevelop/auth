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
@TableName("t_table")
public class Table extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -3494423245247462542L;

    /** 数据源ID */
    private String dataSourceId;

    /** 表ID */
    @TableId(type = IdType.INPUT)
    private String tableId;

    /** 表名 */
    private String tableName;

    /** 注释 */
    private String remark;

    /** 是否使用 */
    private Boolean toUse;

    /** 补充信息 */
    private String additionalInfo;
}
