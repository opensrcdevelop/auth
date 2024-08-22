package cn.opensrcdevelop.auth.biz.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * 授权条件
 */
@Data
@TableName("t_authorize_cond")
public class AuthorizeCondition implements Serializable {

    @Serial
    private static final long serialVersionUID = -269068504145355704L;

    /** 授权ID */
    private String authorizeId;

    /** 权限表达式ID */
    private String permissionExpId;
}
