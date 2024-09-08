package cn.opensrcdevelop.common.cache.annoation;

import java.lang.annotation.*;

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface CacheExpire {

    /** 缓存过期时间（秒）：spel 表达式 或 常量值 */
    String value();
}
