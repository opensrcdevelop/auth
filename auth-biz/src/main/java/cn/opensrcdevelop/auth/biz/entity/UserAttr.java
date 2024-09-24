package cn.opensrcdevelop.auth.biz.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 用户属性
 */
@Data
@TableName("t_user_attr")
@JsonSerialize
public class UserAttr implements Serializable {

    @Serial
    private static final long serialVersionUID = -6383429043764059073L;

    @TableId(type = IdType.INPUT)
    private String attrId;

    private String attrKey;

    private String attrName;

    private String attrDataType;

    /** 扩展属性标记 */
    private Boolean extAttrFlg;

    /** 是否在用户列表显示 */
    private Boolean userLstDisplay;

    /** 显示顺序 */
    @TableField(insertStrategy = FieldStrategy.ALWAYS, updateStrategy = FieldStrategy.ALWAYS)
    private Integer displaySeq;

    /** 显示宽度 */
    private Integer displayWidth;

    /** 用户可见 */
    private Boolean userVisible;

    /** 用户可编辑 */
    private Boolean userEditable;

    @TableField(exist = false)
    private String attrValue;
}
