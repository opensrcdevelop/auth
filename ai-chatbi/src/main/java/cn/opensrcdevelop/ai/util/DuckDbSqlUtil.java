package cn.opensrcdevelop.ai.util;

/**
 * DuckDB SQL 工具类
 * <p>
 * 提供 DuckDB 表名转义、引用等功能
 */
public final class DuckDbSqlUtil {

    private DuckDbSqlUtil() {
    }

    /**
     * 转义 SQL 字符串中的特殊字符
     * <p>
     * 用于 DuckDB 等数据库的表名/字段名转义，转义单引号和反斜杠
     *
     * @param input
     *            输入字符串
     * @return 转义后的字符串
     */
    public static String escapeString(String input) {
        if (input == null) {
            return "";
        }
        return input.replace("\\", "\\\\").replace("'", "\\'");
    }

    /**
     * 为表名添加双引号转义
     * <p>
     * 用于 DuckDB 等数据库，表名存储时添加双引号，与字段存储保持一致
     *
     * @param tableName
     *            原始表名
     * @return 带双引号的表名
     */
    public static String quoteTableName(String tableName) {
        if (tableName == null) {
            return "";
        }
        return "\"" + tableName + "\"";
    }

    /**
     * 去除表名的双引号
     * <p>
     * t_table.tableName 存储时带双引号，此方法用于比较时去除双引号获取原始表名
     *
     * @param tableName
     *            带引号的表名
     * @return 原始表名
     */
    public static String unquoteTableName(String tableName) {
        if (tableName == null) {
            return "";
        }
        // 去除首尾双引号
        if (tableName.startsWith("\"") && tableName.endsWith("\"")) {
            return tableName.substring(1, tableName.length() - 1);
        }
        return tableName;
    }
}
