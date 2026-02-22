package cn.opensrcdevelop.auth.biz.enums;

/**
 * 异步任务类型枚举
 */
public enum AsyncTaskType {

    /**
     * 用户导入，并行度为 1
     */
    USER_IMPORT("USER_IMPORT", "用户导入", 1),

    /**
     * 用户导出，并行度为 3
     */
    USER_EXPORT("USER_EXPORT", "用户导出", 3);

    private final String code;
    private final String desc;
    private final int maxParallelism;

    AsyncTaskType(String code, String desc, int maxParallelism) {
        this.code = code;
        this.desc = desc;
        this.maxParallelism = maxParallelism;
    }

    public String getCode() {
        return code;
    }

    public String getDesc() {
        return desc;
    }

    public int getMaxParallelism() {
        return maxParallelism;
    }

    public static AsyncTaskType fromCode(String code) {
        for (AsyncTaskType type : values()) {
            if (type.code.equals(code)) {
                return type;
            }
        }
        return null;
    }
}
