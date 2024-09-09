package cn.opensrcdevelop.common.cache.support;

import cn.opensrcdevelop.common.cache.annoation.CacheExpire;
import cn.opensrcdevelop.common.cache.aop.CacheExpireAspect;
import cn.opensrcdevelop.common.exception.ServerException;
import jakarta.annotation.Resource;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.data.redis.cache.RedisCache;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.time.Duration;
import java.util.Objects;

@Component
public class CustomRedisCacheManager extends RedisCacheManager {

    private final SpelExpressionParser spelExpressionParser = new SpelExpressionParser();

    @Resource
    private Environment environment;

    @Resource
    private CacheExpireAspect cacheExpireAspect;

    public CustomRedisCacheManager(RedisCacheWriter cacheWriter, RedisCacheConfiguration defaultCacheConfiguration) {
        super(cacheWriter, defaultCacheConfiguration);
    }

    @Override
    @NonNull
    protected RedisCache createRedisCache(@NonNull String name, RedisCacheConfiguration cacheConfiguration) {
        Method method = cacheExpireAspect.getCacheNameMethodCache().get(name);
        if (Objects.nonNull(method)) {
            try {
                CacheExpire expire = AnnotationUtils.findAnnotation(method, CacheExpire.class);
                if (Objects.nonNull(expire)) {
                    // 1. 解析缓存时间
                    String expireExpression = expire.value();
                    String expressionResolveResult = environment.resolvePlaceholders(expireExpression);
                    Long expireSeconds = spelExpressionParser.parseExpression(expressionResolveResult).getValue(Long.class);
                    if (Objects.nonNull(expireSeconds)) {
                        // 2. 设置缓存时间
                        cacheConfiguration = cacheConfiguration.entryTtl(Duration.ofSeconds(expireSeconds));
                    }
                }
            } catch (Exception e) {
                throw new ServerException("The Cache was given a neither valid value nor a valid expression");
            }
        }
        return super.createRedisCache(name, cacheConfiguration);
    }
}
