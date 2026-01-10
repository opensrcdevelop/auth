package cn.opensrcdevelop.ai.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

@Getter
@RequiredArgsConstructor
public enum DataSourceType {

    ORACLE(
            "Oracle",
            "Oracle SQL",
            "jdbc:oracle:thin:@%s:%s:%s",
            "oracle.jdbc.driver.OracleDriver"), MYSQL(
                    "MySQL",
                    "MySQL SQL",
                    "jdbc:mysql://%s:%s/%s",
                    "com.mysql.cj.jdbc.Driver"), POSTGRESQL(
                            "PostgreSQL",
                            "PostgreSQL SQL",
                            "jdbc:postgresql://%s:%s/%s",
                            "org.postgresql.Driver"), SQLSERVER(
                                    "SQL Server",
                                    "T-SQL (Microsoft SQL Server)",
                                    "jdbc:sqlserver://%s:%s;DatabaseName=%s",
                                    "com.microsoft.sqlserver.jdbc.SQLServerDriver");

    private final String displayName;
    private final String dialectName;
    private final String jdbcUrlFormat;
    private final String driverClassName;

    public String getJdbcUrl(String host, Integer port, String database, String jdbcParams) {
        String baseUrl = jdbcUrlFormat.formatted(host, port, database);
        if (StringUtils.isBlank(jdbcParams)) {
            return baseUrl;
        }
        return baseUrl + jdbcParams;
    }
}
