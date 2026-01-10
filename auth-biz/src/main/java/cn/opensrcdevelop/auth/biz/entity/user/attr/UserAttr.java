package cn.opensrcdevelop.auth.biz.entity.user.attr;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.io.Serial;
import java.io.Serializable;
import lombok.Data;

/**
 * 用户属性
 */
@Data
@TableName("t_user_attr")
@EntityName("用户属性")
@JsonSerialize
public class UserAttr implements Serializable {

    @Serial
    private static final long serialVersionUID = -6383429043764059073L;

    @TableId(type = IdType.INPUT)
    @PropertyName("用户属性ID")
    private String attrId;

    @PropertyName("用户属性 Key")
    private String attrKey;

    @PropertyName("用户属性名称")
    private String attrName;

    @PropertyName("用户属性类型")
    private String attrDataType;

    /** 扩展属性标记 */
    @PropertyName("扩展属性标记")
    private Boolean extAttrFlg;

    /** 是否在用户列表显示 */
    @PropertyName("是否在用户列表显示")
    private Boolean userLstDisplay;

    /** 显示顺序 */
    @PropertyName("显示顺序")
    @TableField(insertStrategy = FieldStrategy.ALWAYS, updateStrategy = FieldStrategy.ALWAYS)
    private Integer displaySeq;

    /** 显示宽度 */
    @PropertyName("显示宽度")
    private Integer displayWidth;

    /** 用户可见 */
    @PropertyName("用户可见")
    private Boolean userVisible;

    /** 用户可编辑 */
    @PropertyName("用户可编辑")
    private Boolean userEditable;

    /** 字典ID */
    @PropertyName("字典ID")
    private String dictId;

    @TableField(exist = false)
    private String attrValue;
}
