package cn.opensrcdevelop.auth.biz.entity.permission;

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

/**
 * 权限表达式模版实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission_exp_template")
@EntityName("限制条件模板")
public class PermissionExpTemplate extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 2004218186765964444L;

    @TableId(type = IdType.INPUT)
    @PropertyName("模板ID")
    private String templateId;

    /** 模版名称 */
    @PropertyName("模板名称")
    private String templateName;

    /** 表达式 */
    @PropertyName("表达式")
    private String expression;

    /** 模版参数配置 */
    @PropertyName("模板参数配置")
    private String templateParamConfigs;

    /** 描述 */
    @PropertyName("描述")
    private String description;
}
