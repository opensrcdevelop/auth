package cn.opensrcdevelop.ai.datasource;

import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.exception.ServerException;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Resource;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * DuckDB 数据源提供者
 * <p>
 * 专门处理 DuckDB 内存数据库的 S3 CSV 数据源创建 使用 CREATE SECRET 方式配置 S3 认证，支持 MinIO 等 S3
 * 兼容存储
 */
@Slf4j
@Component
public class DuckDBDataSourceProvider {

    private static final Map<String, Connection> CONNECTION_CACHE = new ConcurrentHashMap<>();

    @Resource
    @Lazy
    private TableService tableService;

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

    @Value("${csv-ds.storage.s3.use-ssl:true}")
    private boolean s3UseSsl;

    @Value("${csv-ds.storage.s3.url-style:vhost}")
    private String s3UrlStyle;

    /**
     * 创建 DuckDB 连接
     * <p>
     * 使用 CREATE SECRET 方式配置 S3 认证，支持: - MinIO 等 S3 兼容存储 - 自定义端点和 SSL 配置 - ATTACH
     * 语句将 S3 CSV 映射为逻辑表名
     *
     * @param dataSourceConf
     *            数据源配置
     * @return DuckDB 连接
     */
    public Connection createConnection(DataSourceConf dataSourceConf) {
        String dataSourceId = dataSourceConf.getDataSourceId();

        // 1. 检查缓存
        if (CONNECTION_CACHE.containsKey(dataSourceId)) {
            Connection cached = CONNECTION_CACHE.get(dataSourceId);
            try {
                if (!cached.isClosed()) {
                    return cached;
                }
            } catch (SQLException ignored) {
                // 连接已关闭，移除缓存
                CONNECTION_CACHE.remove(dataSourceId);
            }
        }

        // 2. 获取关联的表
        List<Table> tables = tableService.list(
                Wrappers.<Table>lambdaQuery().eq(Table::getDataSourceId, dataSourceId));

        try {
            // 3. 创建 DuckDB 内存连接
            Connection conn = DriverManager.getConnection("jdbc:duckdb:");

            // 4. 创建 S3 Secret（使用 KEY_ID/SECRET 方式）
            createS3Secret(conn);

            // 5. 设置 S3 端点配置（MinIO 等兼容存储）
            configureS3Endpoint(conn);

            // 6. ATTACH 所有 CSV 文件
            attachCsvTables(conn, dataSourceId, tables);

            // 7. 缓存连接
            CONNECTION_CACHE.put(dataSourceId, conn);

            return conn;
        } catch (SQLException ex) {
            log.error("创建 DuckDB 连接失败，数据源ID：{}", dataSourceId, ex);
            throw new ServerException("创建 DuckDB 连接失败", ex);
        }
    }

    /**
     * 创建 S3 Secret
     * <p>
     * 使用 CREATE SECRET 语法配置 S3 认证，这是 DuckDB 官方推荐的方式
     */
    private void createS3Secret(Connection conn) throws SQLException {
        try (Statement stmt = conn.createStatement()) {
            String sql = String.format("""
                    CREATE SECRET IF NOT EXISTS csv_datasource_secret (
                        TYPE s3,
                        KEY_ID '%s',
                        SECRET '%s',
                        REGION '%s'
                    )
                    """,
                    escapeString(s3AccessKey),
                    escapeString(s3SecretKey),
                    escapeString(s3Region));

            stmt.execute(sql);
            log.debug("S3 Secret 创建成功");
        }
    }

    /**
     * 配置 S3 端点（用于 MinIO 等非 AWS S3 兼容存储）
     */
    private void configureS3Endpoint(Connection conn) throws SQLException {
        // 仅在配置了自定义端点时设置
        if (s3Endpoint == null || s3Endpoint.isBlank()) {
            return;
        }

        try (Statement stmt = conn.createStatement()) {
            // 设置自定义端点（MinIO 地址）
            stmt.execute("SET s3_endpoint = '" + escapeString(s3Endpoint) + "'");
            log.debug("S3 endpoint 设置为: {}", s3Endpoint);

            // 设置 SSL 模式（MinIO 默认不使用 TLS）
            stmt.execute("SET s3_use_ssl = " + s3UseSsl);
            log.debug("S3 use_ssl 设置为: {}", s3UseSsl);

            // 设置 URL 风格（MinIO 必须使用 path）
            stmt.execute("SET s3_url_style = '" + escapeString(s3UrlStyle) + "'");
            log.debug("S3 url_style 设置为: {}", s3UrlStyle);
        }
    }

    /**
     * ATTACH CSV 表
     * <p>
     * 将 S3 上的 CSV 文件映射为 DuckDB 中的逻辑表名 路径格式:
     * s3://{bucket}/csv-datasource/{dataSourceId}/{tableName}.csv
     */
    private void attachCsvTables(Connection conn, String dataSourceId, List<Table> tables) throws SQLException {
        if (tables == null || tables.isEmpty()) {
            log.warn("数据源 {} 没有关联的表", dataSourceId);
            return;
        }

        try (Statement stmt = conn.createStatement()) {
            for (Table table : tables) {
                // 路径格式: s3://{bucket}/csv-datasource/{dataSourceId}/{tableName}.csv
                String s3Path = String.format("s3://%s/csv-datasource/%s/%s.csv",
                        s3Bucket,
                        dataSourceId,
                        table.getTableName());

                // 使用双引号转义表名（防止特殊字符导致 SQL 错误）
                String sql = "ATTACH '" + escapeString(s3Path) + "' AS \"" + escapeString(table.getTableName()) + "\"";

                try {
                    stmt.execute(sql);
                    log.info("CSV 表 ATTACH 成功: {} -> {}", table.getTableName(), s3Path);
                } catch (SQLException ex) {
                    log.error("CSV 表 ATTACH 失败: {}", table.getTableName(), ex);
                    throw ex;
                }
            }
        }
    }

    /**
     * 关闭并移除连接
     */
    public void closeConnection(String dataSourceId) {
        Connection conn = CONNECTION_CACHE.remove(dataSourceId);
        if (conn != null) {
            try {
                conn.close();
                log.info("DuckDB 连接已关闭: {}", dataSourceId);
            } catch (SQLException ex) {
                log.warn("关闭 DuckDB 连接失败", ex);
            }
        }
    }

    /**
     * 转义 SQL 字符串中的特殊字符
     */
    private String escapeString(String input) {
        if (input == null) {
            return "";
        }
        // 转义单引号和反斜杠
        return input.replace("\\", "\\\\").replace("'", "\\'");
    }

    /**
     * 获取 DuckDB DataSource
     * <p>
     * 创建连接并包装为 DataSource 接口供 JdbcTemplate 使用
     *
     * @param dataSourceId
     *            数据源ID
     * @return DataSource 接口（内部包装了 DuckDB Connection）
     */
    public DataSource getDataSource(String dataSourceId) {
        DataSourceConf dataSourceConf = new DataSourceConf();
        dataSourceConf.setDataSourceId(dataSourceId);
        Connection conn = createConnection(dataSourceConf);
        return new DuckDBConnectionDataSource(conn);
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
            return null;
        }

        @Override
        public void setLogWriter(PrintWriter out) throws SQLException {
            // 无操作
        }

        @Override
        public void setLoginTimeout(int seconds) throws SQLException {
            // 无操作
        }

        @Override
        public int getLoginTimeout() throws SQLException {
            return 0;
        }

        @Override
        public java.util.logging.Logger getParentLogger() {
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
