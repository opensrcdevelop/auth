package cn.opensrcdevelop.auth.biz.entity.user.attr.dict;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.*;
import java.io.Serial;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 字典实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_dict")
@EntityName("字典")
public class Dict extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -800131729781890772L;

    /** 字典ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("字典ID")
    private String dictId;

    /** 字典名称 */
    @PropertyName("字典名称")
    private String dictName;

    /** 字典标识 */
    @PropertyName("字典标识")
    private String dictCode;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    /** 父字典ID */
    @PropertyName("父字典ID")
    @TableField(updateStrategy = FieldStrategy.ALWAYS)
    private String parentDictId;

    /** 关联的字典数据ID */
    @PropertyName("关联的字典数据ID")
    @TableField(updateStrategy = FieldStrategy.ALWAYS)
    private String relatedDictDataId;
}
