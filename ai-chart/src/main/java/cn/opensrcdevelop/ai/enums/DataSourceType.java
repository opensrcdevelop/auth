package cn.opensrcdevelop.ai.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum DataSourceType {

    ORACLE("Oracle SQL"),
    MYSQL("MySQL SQL"),
    POSTGRESQL("PostgreSQL SQL"),
    SQLSERVER("T-SQL (Microsoft SQL Server)");

    private final String dialectName;
}
