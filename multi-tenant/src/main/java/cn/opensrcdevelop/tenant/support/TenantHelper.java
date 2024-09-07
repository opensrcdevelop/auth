package cn.opensrcdevelop.tenant.support;

import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.tenant.component.MultiTenantProperties;
import cn.opensrcdevelop.tenant.service.TenantService;
import com.baomidou.dynamic.datasource.DynamicRoutingDataSource;
import com.baomidou.dynamic.datasource.toolkit.DynamicDataSourceContextHolder;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.vavr.control.Try;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.flywaydb.core.Flyway;

import javax.sql.DataSource;
import java.net.URL;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.Map;
import java.util.Objects;

@Slf4j
public class TenantHelper {

    private TenantHelper() {}

    private static final String CHECK_DATABASE_EXISTS_SCRIPT = "SELECT 1 FROM pg_database WHERE datname = ?";
    private static final String CREATE_DATABASE_SCRIPT = "CREATE DATABASE ";
    private static final String DROP_DATABASE_SCRIPT = "DROP DATABASE IF EXISTS ";
    private static final String PROP_DEFAULT_ISSUER = "auth.server.default-issuer";
    private static final String PROP_DEFAULT_CONSOLE_URL = "auth.server.default-console-url";
    private static final String PROP_CONSOLE_REDIRECT_PATH = "auth.server.console-redirect-path";
    private static final String FLYWAY_PLACEHOLDER_CONSOLE_REDIRECT_URL = "consoleRedirectUrl";

    /**
     * 创建租户数据库
     *
     * @param tenantCode 租户标识
     */
    public static void createTenantDatabase(String tenantCode) {
        try {
            DynamicRoutingDataSource dynamicRoutingDataSource = SpringContextUtil.getBean(DynamicRoutingDataSource.class);
            MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);
            String tenantDbName = multiTenantProperties.getDbPrefix() + tenantCode;

            try (Connection connection = dynamicRoutingDataSource.getConnection();
                 var checkDbExists = connection.prepareStatement(CHECK_DATABASE_EXISTS_SCRIPT);
                 @SuppressWarnings("all")
                 var createDb = connection.prepareStatement(CREATE_DATABASE_SCRIPT + tenantDbName);
                ) {
                // 1. 创建租户数据库
                // 1.1 检查数据库是否存在
                checkDbExists.setString(1, tenantDbName);
                ResultSet resultSet = checkDbExists.executeQuery();
                if (resultSet.next()) {
                    log.info("租户数据库：{} 已存在", tenantDbName);
                } else {
                    // 1.2 创建租户数据库
                    createDb.execute();
                    log.info("成功创建租户数据库：{}", tenantDbName);

                    // 2. 创建租户数据源
                    var ds = createTenantDataSource(tenantCode);

                    // 3. 执行数据库迁移脚本
                    executeTenantDbMigrations(ds, Map.of(FLYWAY_PLACEHOLDER_CONSOLE_REDIRECT_URL,
                            getTenantConsoleUrl(tenantCode) + SpringContextUtil.getProperty(PROP_CONSOLE_REDIRECT_PATH)));
                }
            }
        } catch (Exception e) {
            throw new ServerException(e);
        }
    }

    /**
     * 创建租户数据源
     *
     * @param tenantCode 租户标识
     * @return 租户数据源
     */
    public static DataSource createTenantDataSource(String tenantCode) {
        DynamicRoutingDataSource dynamicRoutingDataSource = SpringContextUtil.getBean(DynamicRoutingDataSource.class);
        MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);

        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setDriverClassName(multiTenantProperties.getDb().getDriverClassName());
        hikariConfig.setJdbcUrl(multiTenantProperties.getDb().getBaseUrl() + multiTenantProperties.getDbPrefix() + tenantCode);
        hikariConfig.setUsername(multiTenantProperties.getDb().getUsername());
        hikariConfig.setPassword(multiTenantProperties.getDb().getPassword());
        hikariConfig.setPoolName(tenantCode);

        var config = multiTenantProperties.getHikariCpConfig();
        CommonUtil.callSetWithCheck(Objects::nonNull, hikariConfig::setAutoCommit, config::getIsAutoCommit);
        CommonUtil.callSetWithCheck(Objects::nonNull, hikariConfig::setMaximumPoolSize, config::getMaximumPoolSize);
        CommonUtil.callSetWithCheck(Objects::nonNull, hikariConfig::setConnectionTimeout, config::getConnectionTimeout);
        CommonUtil.callSetWithCheck(Objects::nonNull, hikariConfig::setMinimumIdle, config::getMinimumIdle);
        CommonUtil.callSetWithCheck(Objects::nonNull, hikariConfig::setIdleTimeout, config::getIdleTimeout);
        CommonUtil.callSetWithCheck(Objects::nonNull, hikariConfig::setMaxLifetime, config::getMaxLifetime);
        hikariConfig.validate();
        var ds = new HikariDataSource(hikariConfig);
        log.info("成功创建租户数据源：{}", tenantCode);
        dynamicRoutingDataSource.addDataSource(tenantCode, ds);
        return ds;
    }

    /**
     * 切换租户数据源
     *
     * @param tenantCode 租户标识
     */
    public static void switchTenantDs(String tenantCode) {
        String currentTenantDs = DynamicDataSourceContextHolder.peek();
        if (!StringUtils.equals(currentTenantDs, tenantCode)) {
            log.debug("切换至租户数据源：{}", tenantCode);
            DynamicRoutingDataSource dynamicRoutingDataSource = SpringContextUtil.getBean(DynamicRoutingDataSource.class);
            if (!dynamicRoutingDataSource.getDataSources().containsKey(tenantCode)) {
                createTenantDataSource(tenantCode);
            }
            // 切换数据源
            DynamicDataSourceContextHolder.push(tenantCode);
        }
    }

    /**
     * 移除租户数据源
     *
     * @param tenantCode 租户标识
     */
    public static void removeTenantDs(String tenantCode) {
        DynamicRoutingDataSource dynamicRoutingDataSource = SpringContextUtil.getBean(DynamicRoutingDataSource.class);
        dynamicRoutingDataSource.removeDataSource(tenantCode);
    }

    /**
     * 清除租户线程上下文
     */
    public static void clearTenantContext() {
        TenantContext.remove();
    }

    /**
     * 清空租户数据源线程上下文
     */
    public static void clearTenantDsContext() {
        DynamicDataSourceContextHolder.clear();
    }

    /**
     * 判断租户是否存在
     *
     * @param tenantCode 租户标识
     * @return 是否存在
     */
    public static boolean tenantExists(String tenantCode) {
        MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);
        switchTenantDs(multiTenantProperties.getDefaultTenant());
        TenantService tenantService = SpringContextUtil.getBean(TenantService.class);
        return StringUtils.isNotBlank(tenantCode) && tenantService.exists(tenantCode);
    }

    /**
     * 删除租户数据库
     *
     * @param tenantCode 租户标识
     */
    public static void removeTenantDatabase(String tenantCode) {
        DynamicRoutingDataSource dynamicRoutingDataSource = SpringContextUtil.getBean(DynamicRoutingDataSource.class);
        MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);
        String tenantDbName = multiTenantProperties.getDbPrefix() + tenantCode;

        try (Connection connection = dynamicRoutingDataSource.getConnection();
             @SuppressWarnings("all")
             var deleteDb = connection.prepareStatement(DROP_DATABASE_SCRIPT + tenantDbName);
        ) {
            // 1. 移除租户数据源
            dynamicRoutingDataSource.removeDataSource(tenantCode);

            // 2. 删除租户数据库
            deleteDb.execute();
            log.info("成功删除租户数据库：{}", tenantDbName);
        } catch (Exception e) {
            throw new ServerException(e);
        }
    }

    /**
     * 执行租户数据库迁移脚本
     *
     * @param dataSource 数据源
     * @param placeHolders 参数
     */
    public static void executeTenantDbMigrations(DataSource dataSource, Map<String, String> placeHolders) {
        MultiTenantProperties multiTenantProperties = SpringContextUtil.getBean(MultiTenantProperties.class);
        Flyway flyway = Flyway.configure()
                .dataSource(dataSource)
                .locations(multiTenantProperties.getFlywayLocation())
                .baselineOnMigrate(true)
                .cleanDisabled(true)
                .placeholders(placeHolders)
                .load();
        flyway.migrate();
    }

    /**
     * 获取租户发行者
     *
     * @param tenantCode 租户标识
     * @return 租户发行者
     */
    public static String getTenantIssuer(String tenantCode) {
        return Try.of(() -> {
            // 添加租户子域名
            URL tmpurl = new URL(SpringContextUtil.getProperty(PROP_DEFAULT_ISSUER));
            return String.format(CommonConstants.URL_FORMAT, tmpurl.getProtocol(), tenantCode + "." + tmpurl.getAuthority());
        }).getOrElseThrow(ServerException::new);
    }

    /**
     * 获取租户控制台 URL
     *
     * @param tenantCode 租户标识
     * @return 租户控制台 URL
     */
    public static String getTenantConsoleUrl(String tenantCode) {
        return Try.of(() -> {
            // 添加租户子域名
            URL tmpurl = new URL(SpringContextUtil.getProperty(PROP_DEFAULT_CONSOLE_URL));
            return String.format(CommonConstants.URL_FORMAT, tmpurl.getProtocol(), tenantCode + "." + tmpurl.getAuthority());
        }).getOrElseThrow(ServerException::new);
    }
}
