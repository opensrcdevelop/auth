package cn.opensrcdevelop.auth.biz.entity.user.attr.dict;

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
@TableName("t_dict_data")
@EntityName("字典数据")
public class DictData extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 5234755644451878931L;

    /** 字典数据ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("字典数据ID")
    private String dataId;

    /** 字典ID */
    @PropertyName("字典ID")
    private String dictId;

    /** 字典数据标签 */
    @PropertyName("字典数据标签")
    private String dataLabel;

    /** 字典数据值 */
    @PropertyName("字典数据值")
    private String dataValue;

    /** 是否启用 */
    @PropertyName("是否启用")
    private Boolean enable;

    /** 显示顺序 */
    @PropertyName("显示顺序")
    private Integer displaySeq;
}
