package cn.opensrcdevelop.auth.biz.entity;

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
@TableName("t_dict_data")
public class DictData extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 5234755644451878931L;

    /** 字典数据ID */
    @TableId(type = IdType.INPUT)
    private String dataId;

    /** 字典ID */
    private String dictId;

    /** 字典数据标签 */
    private String dataLabel;

    /** 字典数据值 */
    private String dataValue;

    /** 是否启用 */
    private Boolean enable;

    /** 显示顺序 */
    private Integer displaySeq;
}
