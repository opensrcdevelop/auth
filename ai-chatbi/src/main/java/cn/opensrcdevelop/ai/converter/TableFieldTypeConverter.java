package cn.opensrcdevelop.ai.converter;

import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.enums.TableFieldType;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

@Component
public class TableFieldTypeConverter {

    private static final Map<String, TableFieldType> POSTGRESQL_TYPE_MAPPINGS = new HashMap<>();
    private static final Map<String, TableFieldType> MYSQL_TYPE_MAPPINGS = new HashMap<>();
    private static final Map<String, TableFieldType> ORACLE_TYPE_MAPPINGS = new HashMap<>();
    private static final Map<String, TableFieldType> SQLSERVER_TYPE_MAPPINGS = new HashMap<>();

    // PostgreSQL 类型映射
    static {
        POSTGRESQL_TYPE_MAPPINGS.put("varchar", TableFieldType.STRING);
        POSTGRESQL_TYPE_MAPPINGS.put("char", TableFieldType.STRING);
        POSTGRESQL_TYPE_MAPPINGS.put("text", TableFieldType.STRING);
        POSTGRESQL_TYPE_MAPPINGS.put("int", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("integer", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("bigint", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("float", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("double", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("decimal", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("numeric", TableFieldType.NUMBER);
        POSTGRESQL_TYPE_MAPPINGS.put("bit", TableFieldType.BOOLEAN);
        POSTGRESQL_TYPE_MAPPINGS.put("boolean", TableFieldType.BOOLEAN);
        POSTGRESQL_TYPE_MAPPINGS.put("date", TableFieldType.DATETIME);
        POSTGRESQL_TYPE_MAPPINGS.put("timestamp", TableFieldType.DATETIME);
        POSTGRESQL_TYPE_MAPPINGS.put("time", TableFieldType.DATETIME);
    }

    // ORACLE 类型映射
    static {
        ORACLE_TYPE_MAPPINGS.put("varchar", TableFieldType.STRING);
        ORACLE_TYPE_MAPPINGS.put("char", TableFieldType.STRING);
        ORACLE_TYPE_MAPPINGS.put("text", TableFieldType.STRING);
        ORACLE_TYPE_MAPPINGS.put("int", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("integer", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("bigint", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("float", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("double", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("decimal", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("numeric", TableFieldType.NUMBER);
        ORACLE_TYPE_MAPPINGS.put("bit", TableFieldType.BOOLEAN);
        ORACLE_TYPE_MAPPINGS.put("boolean", TableFieldType.BOOLEAN);
        ORACLE_TYPE_MAPPINGS.put("date", TableFieldType.DATETIME);
        ORACLE_TYPE_MAPPINGS.put("timestamp", TableFieldType.DATETIME);
        ORACLE_TYPE_MAPPINGS.put("time", TableFieldType.DATETIME);
    }

    // MySQL 类型映射
    static {
        MYSQL_TYPE_MAPPINGS.put("varchar", TableFieldType.STRING);
        MYSQL_TYPE_MAPPINGS.put("text", TableFieldType.STRING);
        MYSQL_TYPE_MAPPINGS.put("int", TableFieldType.NUMBER);
        MYSQL_TYPE_MAPPINGS.put("integer", TableFieldType.NUMBER);
        MYSQL_TYPE_MAPPINGS.put("bigint", TableFieldType.NUMBER);
        MYSQL_TYPE_MAPPINGS.put("float", TableFieldType.NUMBER);
        MYSQL_TYPE_MAPPINGS.put("double", TableFieldType.NUMBER);
        MYSQL_TYPE_MAPPINGS.put("decimal", TableFieldType.NUMBER);
        MYSQL_TYPE_MAPPINGS.put("bit", TableFieldType.BOOLEAN);
        MYSQL_TYPE_MAPPINGS.put("boolean", TableFieldType.BOOLEAN);
        MYSQL_TYPE_MAPPINGS.put("date", TableFieldType.DATETIME);
        MYSQL_TYPE_MAPPINGS.put("timestamp", TableFieldType.DATETIME);
        MYSQL_TYPE_MAPPINGS.put("time", TableFieldType.DATETIME);
    }

    // SQL Server 类型映射
    static {
        SQLSERVER_TYPE_MAPPINGS.put("varchar", TableFieldType.STRING);
        SQLSERVER_TYPE_MAPPINGS.put("text", TableFieldType.STRING);
        SQLSERVER_TYPE_MAPPINGS.put("int", TableFieldType.NUMBER);
        SQLSERVER_TYPE_MAPPINGS.put("integer", TableFieldType.NUMBER);
        SQLSERVER_TYPE_MAPPINGS.put("bigint", TableFieldType.NUMBER);
        SQLSERVER_TYPE_MAPPINGS.put("float", TableFieldType.NUMBER);
        SQLSERVER_TYPE_MAPPINGS.put("double", TableFieldType.NUMBER);
        SQLSERVER_TYPE_MAPPINGS.put("decimal", TableFieldType.NUMBER);
        SQLSERVER_TYPE_MAPPINGS.put("bit", TableFieldType.BOOLEAN);
        SQLSERVER_TYPE_MAPPINGS.put("boolean", TableFieldType.BOOLEAN);
        SQLSERVER_TYPE_MAPPINGS.put("date", TableFieldType.DATETIME);
        SQLSERVER_TYPE_MAPPINGS.put("timestamp", TableFieldType.DATETIME);
        SQLSERVER_TYPE_MAPPINGS.put("time", TableFieldType.DATETIME);
    }

    /**
     * 将数据库数据类型转换为 TableFieldType
     *
     * @param dataSourceType 数据源信息
     * @param dataType 数据类型
     * @return TableFieldType
     */
    public TableFieldType convert(DataSourceType dataSourceType, String dataType) {
        Map<String, TableFieldType> typeMappings = switch (dataSourceType) {
            case POSTGRESQL -> POSTGRESQL_TYPE_MAPPINGS;
            case MYSQL -> MYSQL_TYPE_MAPPINGS;
            case ORACLE -> ORACLE_TYPE_MAPPINGS;
            case SQLSERVER -> SQLSERVER_TYPE_MAPPINGS;
        };

        return typeMappings.getOrDefault(dataType.toLowerCase(), TableFieldType.STRING);
    }
}
