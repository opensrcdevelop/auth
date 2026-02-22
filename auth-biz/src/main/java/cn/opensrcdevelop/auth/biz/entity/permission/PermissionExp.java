package cn.opensrcdevelop.auth.biz.entity.permission;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 权限表达式实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission_exp")
@EntityName("限制条件")
public class PermissionExp extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -2560065750602545879L;

    /** 表达式ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("表达式ID")
    private String expressionId;

    /** 表达式名称 */
    @PropertyName("限制条件名称")
    private String expressionName;

    /** 表达式 */
    @PropertyName("JEXL 表达式")
    private String expression;

    /** 模板ID */
    @PropertyName("模板ID")
    private String templateId;

    /** 模板参数 */
    @PropertyName("模板参数")
    private String templateParams;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    @TableField(exist = false)
    private PermissionExpTemplate permissionExpTemplate;
}
