package cn.opensrcdevelop.auth.biz.enums;

/**
 * 异步任务状态枚举
 */
public enum AsyncTaskStatus {

    /**
     * 等待中
     */
    PENDING("PENDING", "等待中"),

    /**
     * 执行中
     */
    RUNNING("RUNNING", "执行中"),

    /**
     * 成功
     */
    SUCCESS("SUCCESS", "成功"),

    /**
     * 失败
     */
    FAILED("FAILED", "失败"),

    /**
     * 已取消
     */
    CANCELLED("CANCELLED", "已取消");

    private final String code;
    private final String desc;

    AsyncTaskStatus(String code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    public String getCode() {
        return code;
    }

    public String getDesc() {
        return desc;
    }

    public static AsyncTaskStatus fromCode(String code) {
        for (AsyncTaskStatus status : values()) {
            if (status.code.equals(code)) {
                return status;
            }
        }
        return null;
    }
}
