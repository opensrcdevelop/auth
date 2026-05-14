package cn.opensrcdevelop.ai.datasource;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.baomidou.dynamic.datasource.DynamicRoutingDataSource;
import com.baomidou.dynamic.datasource.toolkit.DynamicDataSourceContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import jakarta.annotation.Resource;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import javax.sql.DataSource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class DataSourceManager {

    @Resource
    @Lazy
    private DataSourceConfService dataSourceConfService;

    @Resource
    @Lazy
    private TableService tableService;

    private static final Map<String, DataSource> DATA_SOURCE_CACHE = new ConcurrentHashMap<>();

    private static final Map<String, Connection> DUCKDB_CONNECTION_CACHE = new ConcurrentHashMap<>();

    @Value("${csv-ds.storage.s3.endpoint:}")
    private String s3Endpoint;

    @Value("${csv-ds.storage.s3.region:}")
    private String s3Region;

    @Value("${csv-ds.storage.s3.bucket:}")
    private String s3Bucket;

    @Value("${csv-ds.storage.s3.access-key:}")
    private String s3AccessKey;

    @Value("${csv-ds.storage.s3.secret-key:}")
    private String s3SecretKey;

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

        // 4. DUCKDB 类型特殊处理 - 使用原生 Connection 而非 HikariDataSource
        if (dataSourceType == DataSourceType.DUCKDB) {
            return getDuckDBDataSource(dataSourceId);
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

        // 关闭 DuckDB 连接并移除缓存
        Connection duckDBConn = DUCKDB_CONNECTION_CACHE.remove(dataSourceId);
        if (duckDBConn != null) {
            try {
                duckDBConn.close();
            } catch (SQLException ex) {
                log.warn("关闭 DuckDB 连接失败", ex);
            }
        }
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

    /**
     * 获取 DuckDB 数据源
     *
     * @param dataSourceId
     *            数据源ID
     * @return DuckDB 数据源
     */
    private DataSource getDuckDBDataSource(String dataSourceId) {
        // 1. 检查缓存中是否存在 DuckDB 连接
        if (DUCKDB_CONNECTION_CACHE.containsKey(dataSourceId)) {
            return new DuckDBConnectionDataSource(DUCKDB_CONNECTION_CACHE.get(dataSourceId));
        }

        // 2. 获取关联的表
        List<Table> tables = tableService.list(
                Wrappers.<Table>lambdaQuery().eq(Table::getDataSourceId, dataSourceId));

        // 3. 创建 DuckDB 内存连接
        try {
            Connection conn = DriverManager.getConnection("jdbc:duckdb:");

            // 4. 设置 S3 凭证 (D-01)
            try (Statement stmt = conn.createStatement()) {
                stmt.execute("SET s3_access_key_id = '" + s3AccessKey + "'");
                stmt.execute("SET s3_secret_access_key = '" + s3SecretKey + "'");

                // 5. ATTACH 所有 CSV 文件 (D-02, D-03)
                for (Table table : tables) {
                    // 路径格式: s3://{bucket}/csv-datasource/{dataSourceId}/{tableName}.csv
                    String s3Path = "s3://" + s3Bucket + "/csv-datasource/" + dataSourceId + "/" + table.getTableName()
                            + ".csv";
                    // 表名双引号转义 (D-04)
                    stmt.execute("ATTACH '" + s3Path + "' AS \"" + table.getTableName() + "\"");
                }
            }

            // 6. 缓存连接
            DUCKDB_CONNECTION_CACHE.put(dataSourceId, conn);

            // 7. 返回包装后的 DataSource
            return new DuckDBConnectionDataSource(conn);
        } catch (SQLException ex) {
            log.error("创建 DuckDB 连接失败，数据源ID：{}", dataSourceId, ex);
            throw new ServerException("创建 DuckDB 连接失败", ex);
        }
    }

    /**
     * DuckDB 连接包装器，将原生 Connection 适配为 DataSource 接口 用于 JdbcTemplate 操作
     */
    private static class DuckDBConnectionDataSource implements DataSource {
        private final Connection connection;

        DuckDBConnectionDataSource(Connection connection) {
            this.connection = connection;
        }

        @Override
        public Connection getConnection() throws SQLException {
            return connection;
        }

        @Override
        public Connection getConnection(String username, String password) throws SQLException {
            return connection;
        }

        @Override
        public PrintWriter getLogWriter() throws SQLException {
            // DuckDB Connection 不支持此操作，返回 null
            return null;
        }

        @Override
        public void setLogWriter(PrintWriter out) throws SQLException {
            // DuckDB Connection 不支持此操作，无操作
        }

        @Override
        public void setLoginTimeout(int seconds) throws SQLException {
            // DuckDB Connection 不支持此操作，使用默认值
        }

        @Override
        public int getLoginTimeout() throws SQLException {
            // DuckDB Connection 不支持此操作，返回默认值 0
            return 0;
        }

        @Override
        public java.util.logging.Logger getParentLogger() {
            // DataSource 接口要求实现，但 DuckDB Connection 不支持
            return null;
        }

        @Override
        public <T> T unwrap(Class<T> iface) throws SQLException {
            if (iface.isInstance(this)) {
                return iface.cast(this);
            }
            return connection.unwrap(iface);
        }

        @Override
        public boolean isWrapperFor(Class<?> iface) throws SQLException {
            return iface.isInstance(this) || connection.isWrapperFor(iface);
        }
    }
}
