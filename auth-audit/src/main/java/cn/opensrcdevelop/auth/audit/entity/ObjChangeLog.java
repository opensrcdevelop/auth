package cn.opensrcdevelop.auth.audit.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

@TableName("t_obj_change_log")
@Data
public class ObjChangeLog {

    @TableId(type = IdType.AUTO)
    private Long id;

    /** 审计 ID */
    private String auditId;

    /** java 类型 */
    private String javaType;

    /** 对象ID */
    private String objId;

    /** 变更前 */
    private String before;

    /** 变更后 */
    private String after;
}
