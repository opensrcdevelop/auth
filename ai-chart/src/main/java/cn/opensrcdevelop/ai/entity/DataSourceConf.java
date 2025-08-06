package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_data_source_conf")
public class DataSourceConf extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 6484210649647474804L;

    /** 数据源ID */
    @TableId(type = IdType.INPUT)
    private String dataSourceId;

    /** 数据源名称 */
    private String dataSourceName;

    /** 数据源类型 */
    private String dataSourceType;

    /** 数据库名称 */
    private String database;

    /** 主机地址 */
    private String host;

    /** 端口号 */
    private String port;

    /** 用户名 */
    private String username;

    /** 密码 */
    private String password;

    /** 是否启用 */
    private Boolean enabled;
}
