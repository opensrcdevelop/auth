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
 * 系统设置实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_sys_setting")
public class SystemSetting extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -7850798650932860588L;

    /** 系统设置 key */
    @TableId(type = IdType.INPUT)
    private String key;

    /** 系统设置 value */
    private String value;

    /** 系统设置描述 */
    private String description;
}
