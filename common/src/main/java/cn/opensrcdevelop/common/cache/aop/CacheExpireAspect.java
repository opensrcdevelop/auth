package cn.opensrcdevelop.common.cache.aop;

import cn.opensrcdevelop.common.cache.annoation.CacheExpire;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;
import lombok.Getter;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Getter
@Aspect
@Order(Ordered.LOWEST_PRECEDENCE - 10)
@Component
public class CacheExpireAspect {

    private final Map<String, Method> cacheNameMethodCache = new ConcurrentHashMap<>();

    @Pointcut("@annotation(cn.opensrcdevelop.common.cache.annoation.CacheExpire)")
    public void pointCut() {
    }

    @Before("pointCut()")
    public void before(JoinPoint joinPoint) {
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
        CacheExpire timeToLive = AnnotationUtils.findAnnotation(method, CacheExpire.class);
        Cacheable cacheable = AnnotationUtils.findAnnotation(method, Cacheable.class);

        // 缓存需要设置过期时间的缓存方法
        if (Objects.nonNull(timeToLive) && Objects.nonNull(cacheable) && cacheable.cacheNames().length > 0) {
            Stream.of(cacheable.cacheNames())
                    .filter(cacheName -> !cacheNameMethodCache.containsKey(cacheName))
                    .forEach(cacheName -> cacheNameMethodCache.put(cacheName, method));
        }
    }
}
