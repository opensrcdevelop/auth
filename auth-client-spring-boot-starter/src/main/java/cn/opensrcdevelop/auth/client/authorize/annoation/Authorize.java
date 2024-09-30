package cn.opensrcdevelop.auth.client.authorize.annoation;

import org.springframework.security.access.prepost.PreAuthorize;

import java.lang.annotation.*;

@Target({ ElementType.METHOD, ElementType.TYPE })
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
