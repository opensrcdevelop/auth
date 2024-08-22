package cn.opensrcdevelop.auth.biz.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

/**
 * 权限表达式实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_permission_exp")
public class PermissionExp extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -2560065750602545879L;

    /** 表达式ID */
    @TableId(type = IdType.INPUT)
    private String expressionId;

    /** 表达式名称 */
    private String expressionName;

    /** 表达式 */
    private String expression;

    /** 描述 */
    private String description;
}
