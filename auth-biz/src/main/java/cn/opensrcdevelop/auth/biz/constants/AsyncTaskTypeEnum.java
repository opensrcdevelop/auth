package cn.opensrcdevelop.auth.biz.constants;

import lombok.Getter;

/**
 * 异步任务类型枚举
 */
@Getter
public enum AsyncTaskTypeEnum {

    /**
     * 用户导入，并行度为 1
     */
    USER_IMPORT("USER_IMPORT", "用户导入", 1),

    /**
     * 用户导出，并行度为 3
     */
    USER_EXPORT("USER_EXPORT", "用户导出", 3),

    /**
     * 示例 SQL 同步，并行度为 1
     */
    SAMPLE_SQL_SYNC("SAMPLE_SQL_SYNC", "示例 SQL 同步", 1),

    /**
     * 示例 SQL 重建索引，并行度为 1
     */
    SAMPLE_SQL_REBUILD("SAMPLE_SQL_REBUILD", "示例 SQL 重建索引", 1),

    /**
     * CSV 文件解析，并行度为 2
     */
    CSV_PARSE("CSV_PARSE", "CSV 文件解析", 2);

    private final String code;
    private final String desc;
    private final int maxParallelism;

    AsyncTaskTypeEnum(String code, String desc, int maxParallelism) {
        this.code = code;
        this.desc = desc;
        this.maxParallelism = maxParallelism;
    }

    public static AsyncTaskTypeEnum fromCode(String code) {
        for (AsyncTaskTypeEnum type : values()) {
            if (type.code.equals(code)) {
                return type;
            }
        }
        return null;
    }
}
