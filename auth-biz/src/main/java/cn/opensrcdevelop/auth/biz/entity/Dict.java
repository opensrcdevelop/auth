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
 * 字典实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_dict")
public class Dict extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -800131729781890772L;

    /** 字典ID */
    @TableId(type = IdType.INPUT)
    private String dictId;

    /** 字典名称 */
    private String dictName;

    /** 字典标识 */
    private String dictCode;

    /** 描述 */
    private String description;
}
