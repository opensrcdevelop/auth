package cn.opensrcdevelop.auth.biz.service.asynctask;

import java.lang.annotation.*;

/**
 * 异步任务执行器注解
 * 标记在实现 {@link AsyncTaskExecutor} 接口的类上
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AsyncTaskExecutorAnno {

    /**
     * 任务类型代码
     *
     * @return 任务类型代码
     */
    String taskType();
}
