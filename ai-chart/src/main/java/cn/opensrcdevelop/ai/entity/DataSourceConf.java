package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

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

    /** 数据源描述 */
    private String description;

    /** 数据库名称 */
    private String database;

    /** 模式 */
    private String schema;

    /** 主机地址 */
    private String host;

    /** 端口号 */
    private Integer port;

    /** 用户名 */
    private String username;

    /** 密码 */
    private String password;

    /** jdbc参数 */
    private String jdbcParams;

    /** 是否启用 */
    private Boolean enabled;

    /** 最后同步表时间 */
    private LocalDateTime lastSyncTableTime;

    /** 同步表次数 */
    private Long syncTableCount;

    /** 是否系统数据源 */
    private Boolean systemDs;
}
