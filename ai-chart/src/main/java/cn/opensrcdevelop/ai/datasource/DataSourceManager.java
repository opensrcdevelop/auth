package cn.opensrcdevelop.ai.datasource;


import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
@Component
@RequiredArgsConstructor
public class DataSourceManager {

    private final DataSourceConfService dataSourceConfService;

    private static final Map<String, DataSource> DATA_SOURCE_CACHE = new ConcurrentHashMap<>();

    /**
     * 获取数据源
     *
     * @param dataSourceId 数据源ID
     * @return 数据源
     */
    public synchronized DataSource getDataSource(String dataSourceId) {
        // 1. 检查缓存中是否存在数据源
        if (DATA_SOURCE_CACHE.containsKey(dataSourceId)) {
            return DATA_SOURCE_CACHE.get(dataSourceId);
        }

        // 2. 获取数据源配置
        DataSourceConf dataSourceConf = dataSourceConfService
                .getOne(Wrappers.<DataSourceConf>lambdaQuery()
                        .eq(DataSourceConf::getDataSourceId, dataSourceId)
                        .eq(DataSourceConf::getEnabled, true));
        if (Objects.isNull(dataSourceConf)) {
            throw new BizException(MessageConstants.AI_DATASOURCE_MSG_1000);
        }

        // 3. 创建数据源
        DataSourceType dataSourceType = DataSourceType.valueOf(dataSourceConf.getDataSourceType());
        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setDriverClassName(getDriverClassName(dataSourceType));
        hikariConfig.setJdbcUrl(getJdbcUrl(dataSourceType, dataSourceConf.getHost(), dataSourceConf.getPort(), dataSourceConf.getDatabase()));
        hikariConfig.setUsername(dataSourceConf.getUsername());
        hikariConfig.setPassword(dataSourceConf.getPassword());
        HikariDataSource dataSource = new HikariDataSource(hikariConfig);
        DATA_SOURCE_CACHE.put(dataSourceId, dataSource);
        return dataSource;
    }

    /**
     * 获取数据库连接
     *
     * @param dataSourceId 数据源ID
     * @return 数据库连接
     */
    public Connection getConnection(String dataSourceId) {
        DataSource dataSource = getDataSource(dataSourceId);
        try {
            return dataSource.getConnection();
        } catch (SQLException ex) {
            log.error("获取数据库连接失败，数据源ID：{}", dataSourceId);
            throw new ServerException(ex);
        }
    }

    /**
     * 移除数据源
     *
     * @param dataSourceId 数据源ID
     */
    public void removeDataSource(String dataSourceId) {
        DATA_SOURCE_CACHE.remove(dataSourceId);
    }

    /**
     * 获取 JdbcTemplate
     *
     * @param dataSourceId 数据源ID
     * @return JdbcTemplate
     */
    public JdbcTemplate getJdbcTemplate(String dataSourceId) {
        DataSource dataSource = getDataSource(dataSourceId);
        return new JdbcTemplate(dataSource);
    }

    /**
     *  获取数据源类型
     *
     * @param dataSourceId 数据源ID
     * @return 数据源类型
     */
    public DataSourceType getDataSourceType(String dataSourceId) {
        DataSourceConf dataSourceConf = dataSourceConfService.getById(dataSourceId);
        if (Objects.isNull(dataSourceConf)) {
            return null;
        }

        return DataSourceType.valueOf(dataSourceConf.getDataSourceType());
    }

    private String getJdbcUrl(DataSourceType dataSourceType, String host, String port, String database) {
        return switch (dataSourceType) {
            case ORACLE -> String.format("jdbc:oracle:thin:@%s:%s:%s", host, port, database);
            case MYSQL -> String.format("jdbc:mysql://%s:%s/%s?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&remarks=true&useInformationSchema=true", host, port, database);
            case POSTGRESQL -> String.format("jdbc:postgresql://%s:%s/%s", host, port, database);
            case SQLSERVER -> String.format("jdbc:sqlserver://%s:%s;DatabaseName=%s", host, port, database);
        };
    }

    private String getDriverClassName(DataSourceType dataSourceType) {
        return switch (dataSourceType) {
            case ORACLE -> "oracle.jdbc.driver.OracleDriver";
            case MYSQL -> "com.mysql.cj.jdbc.Driver";
            case POSTGRESQL -> "org.postgresql.Driver";
            case SQLSERVER -> "com.microsoft.sqlserver.jdbc.SQLServerDriver";
        };
    }
}
