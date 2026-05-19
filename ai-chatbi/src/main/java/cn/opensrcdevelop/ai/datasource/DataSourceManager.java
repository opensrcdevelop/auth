package cn.opensrcdevelop.ai.datasource;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.baomidou.dynamic.datasource.DynamicRoutingDataSource;
import com.baomidou.dynamic.datasource.toolkit.DynamicDataSourceContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import javax.sql.DataSource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class DataSourceManager {

    private final DataSourceConfService dataSourceConfService;
    private final DuckDBDataSourceProvider duckDBDataSourceProvider;

    private static final Map<String, DataSource> DATA_SOURCE_CACHE = new ConcurrentHashMap<>();

    /**
     * 获取数据源
     *
     * @param dataSourceId
     *            数据源ID
     * @return 数据源
     */
    public synchronized DataSource getDataSource(String dataSourceId) {
        // 1. 获取数据源配置
        DataSourceConf dataSourceConf = dataSourceConfService
                .getOne(Wrappers.<DataSourceConf>lambdaQuery()
                        .eq(DataSourceConf::getDataSourceId, dataSourceId)
                        .eq(DataSourceConf::getEnabled, true));
        if (Objects.isNull(dataSourceConf)) {
            throw new BizException(MessageConstants.AI_DATASOURCE_MSG_1000);
        }

        // 2. 判断是否为系统数据源
        if (Boolean.TRUE.equals(dataSourceConf.getSystemDs())) {
            DynamicRoutingDataSource dynamicRoutingDataSource = SpringContextUtil
                    .getBean(DynamicRoutingDataSource.class);
            return dynamicRoutingDataSource.getDataSource(DynamicDataSourceContextHolder.peek());
        }

        // 3. 判断数据类型
        DataSourceType dataSourceType = DataSourceType.valueOf(dataSourceConf.getDataSourceType());

        // 4. DUCKDB 类型特殊处理 - 使用 DuckDBDataSourceProvider 创建连接
        if (dataSourceType == DataSourceType.DUCKDB) {
            return duckDBDataSourceProvider.getDataSource(dataSourceId);
        }

        // 5. 检查缓存中是否存在数据源
        if (DATA_SOURCE_CACHE.containsKey(dataSourceId)) {
            return DATA_SOURCE_CACHE.get(dataSourceId);
        }

        // 6. 创建 HikariCP 数据源
        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setDriverClassName(dataSourceType.getDriverClassName());
        hikariConfig.setJdbcUrl(dataSourceType.getJdbcUrl(dataSourceConf.getHost(), dataSourceConf.getPort(),
                dataSourceConf.getDatabase(), dataSourceConf.getJdbcParams()));
        hikariConfig.setUsername(dataSourceConf.getUsername());
        hikariConfig.setPassword(dataSourceConf.getPassword());
        hikariConfig.setSchema(dataSourceConf.getSchema());
        HikariDataSource dataSource = new HikariDataSource(hikariConfig);
        DATA_SOURCE_CACHE.put(dataSourceId, dataSource);
        return dataSource;
    }

    /**
     * 获取数据库连接
     *
     * @param dataSourceId
     *            数据源ID
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
     * @param dataSourceId
     *            数据源ID
     */
    public void removeDataSource(String dataSourceId) {
        DATA_SOURCE_CACHE.remove(dataSourceId);
        // 清理 DuckDB 临时文件
        duckDBDataSourceProvider.cleanupTempFile(dataSourceId);
    }

    /**
     * 获取 JdbcTemplate
     *
     * @param dataSourceId
     *            数据源ID
     * @return JdbcTemplate
     */
    public JdbcTemplate getJdbcTemplate(String dataSourceId) {
        DataSource dataSource = getDataSource(dataSourceId);
        return new JdbcTemplate(dataSource);
    }

    /**
     * 获取数据源类型
     *
     * @param dataSourceId
     *            数据源ID
     * @return 数据源类型
     */
    public DataSourceType getDataSourceType(String dataSourceId) {
        DataSourceConf dataSourceConf = dataSourceConfService.getById(dataSourceId);
        if (Objects.isNull(dataSourceConf)) {
            return null;
        }

        return DataSourceType.valueOf(dataSourceConf.getDataSourceType());
    }
}
