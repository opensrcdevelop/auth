package cn.opensrcdevelop.auth.biz.entity.resource.group;

import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * 资源组实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_resource_group")
public class ResourceGroup extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -2410922396076977739L;

    /** 资源组ID */
    @TableId(type = IdType.INPUT)
    private String resourceGroupId;

    /** 资源组名 */
    private String resourceGroupName;

    /** 资源组码 */
    private String resourceGroupCode;

    /** 描述 */
    private String description;

    /** 资源集合 */
    @TableField(exist = false)
    private List<Resource> resources;
}
