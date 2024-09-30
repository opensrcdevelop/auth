package cn.opensrcdevelop.auth.biz.annocation;


import java.lang.annotation.*;

@Target({ ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(ResourceLimits.class)
public @interface ResourceLimit {

    /** 限制资源 ID 集合 */
    String[] ids();

    /** ID SpringEL 表达式 */
    String idEl();

    /** ID 是否为 List */
    boolean isList() default false;
}