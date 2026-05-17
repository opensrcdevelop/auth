package cn.opensrcdevelop.ai.datasource;

import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.exception.ServerException;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import javax.sql.DataSource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * DuckDB 数据源提供者
 * <p>
 * 专门处理 DuckDB 内存数据库的 S3 CSV 数据源创建 使用 CREATE SECRET 方式配置 S3 认证，支持 MinIO 等 S3
 * 兼容存储
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class DuckDBDataSourceProvider {

    private static final Map<String, Connection> CONNECTION_CACHE = new ConcurrentHashMap<>();

    private final TableService tableService;

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
            // 3. 创建 DuckDB 内存连接（明确读写模式）
            Properties properties = new Properties();
            properties.setProperty("access_mode", "READ_WRITE");
            Connection conn = DriverManager.getConnection("jdbc:duckdb:", properties);

            // 4. 配置 S3 连接参数（使用旧版 SET 方式）
            configureS3(conn);

            // 5. ATTACH 所有 CSV 文件
            attachCsvTables(conn, dataSourceId, tables);

            // 6. 缓存连接
            CONNECTION_CACHE.put(dataSourceId, conn);

            return conn;
        } catch (SQLException ex) {
            log.error("创建 DuckDB 连接失败，数据源ID：{}", dataSourceId, ex);
            throw new ServerException("创建 DuckDB 连接失败", ex);
        }
    }

    /**
     * 配置 S3 连接参数
     * <p>
     * 使用旧版 SET 方式配置 S3 认证
     */
    private void configureS3(Connection conn) throws SQLException {
        setS3Config(conn, "s3_region", s3Region);
        if (StringUtils.isNotBlank(s3Endpoint)) {
            setS3Config(conn, "s3_endpoint", stripProtocol(s3Endpoint));
        }
        setS3Config(conn, "s3_use_ssl", String.valueOf(s3UseSsl));
        setS3Config(conn, "s3_url_style", s3UrlStyle);
        setS3Config(conn, "s3_access_key_id", s3AccessKey);
        setS3Config(conn, "s3_secret_access_key", s3SecretKey);
    }

    /**
     * 去除 URL 协议前缀
     */
    private String stripProtocol(String url) {
        if (url == null) {
            return "";
        }
        return url.replaceFirst("^https?://", "");
    }

    /**
     * 设置 S3 配置参数
     */
    private void setS3Config(Connection conn, String key, String value) throws SQLException {
        if (StringUtils.isBlank(value)) {
            return;
        }

        String sql = "SET " + key + " = ?";
        try (PreparedStatement ps = conn.prepareStatement(sql)) {
            ps.setString(1, value);
            ps.execute();
        }
    }

    /**
     * 注册 CSV 表
     * <p>
     * 使用 CREATE TABLE ... FROM read_csv_auto() 将 S3 CSV 文件创建为可查询的表 路径格式:
     * s3://{bucket}/{dataSourceId}/{tableName}.csv
     */
    private void attachCsvTables(Connection conn, String dataSourceId, List<Table> tables) throws SQLException {
        if (CollectionUtils.isEmpty(tables)) {
            log.warn("数据源 {} 没有关联的表", dataSourceId);
            return;
        }

        try (Statement stmt = conn.createStatement()) {
            for (Table table : tables) {
                // 路径格式: s3://{bucket}/{dataSourceId}/{tableName}.csv
                // 与 CsvFileServiceImpl.uploadCsv 中的路径一致
                String s3Path = String.format("s3://%s/%s/%s.csv",
                        s3Bucket,
                        dataSourceId,
                        table.getTableName());

                // 使用 CREATE TABLE ... FROM read_csv_auto() 语法创建表
                String sql = "CREATE TABLE \"" + escapeString(table.getTableName()) + "\" AS " +
                        "SELECT * FROM read_csv_auto('" + escapeString(s3Path) + "')";

                try {
                    stmt.execute(sql);
                    log.info("CSV 表创建成功: {} -> {}", table.getTableName(), s3Path);
                } catch (SQLException ex) {
                    log.error("CSV 表创建失败: {}", table.getTableName(), ex);
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
