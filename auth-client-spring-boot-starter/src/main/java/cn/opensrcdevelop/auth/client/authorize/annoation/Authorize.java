package cn.opensrcdevelop.auth.client.authorize.annoation;

import java.lang.annotation.*;
import org.springframework.security.access.prepost.PreAuthorize;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@PreAuthorize("@pms.checkPermission(#root)")
public @interface Authorize {

    /** 权限名称集合 */
    String[] value();

    /** 任意权限匹配 */
    boolean anyMatch() default true;
}
