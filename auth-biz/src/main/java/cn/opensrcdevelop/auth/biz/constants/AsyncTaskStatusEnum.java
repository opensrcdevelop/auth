package cn.opensrcdevelop.auth.biz.constants;

import lombok.Getter;

/**
 * 异步任务状态枚举
 */
@Getter
public enum AsyncTaskStatusEnum {

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

    AsyncTaskStatusEnum(String code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    public static AsyncTaskStatusEnum fromCode(String code) {
        for (AsyncTaskStatusEnum status : values()) {
            if (status.code.equals(code)) {
                return status;
            }
        }
        return null;
    }
}
