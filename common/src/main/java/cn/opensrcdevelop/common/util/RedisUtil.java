package cn.opensrcdevelop.common.util;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.Assert;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

@SuppressWarnings("unused")
public class RedisUtil {

    private RedisUtil() {}

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
    private static final ReentrantLock LOCK = new ReentrantLock();
    private static StringRedisTemplate redisTemplate;
    private static RedissonClient redissonClient;

    public static void setRedisTemplate(StringRedisTemplate stringRedisTemplate) {
        LOCK.lock();
        try {
            if (redisTemplate == null) {
                redisTemplate = stringRedisTemplate;
            }
        } finally {
            LOCK.unlock();
        }
    }

    public static void setRedissonClient(RedissonClient client) {
        LOCK.lock();
        try {
            if (redissonClient == null) {
                redissonClient = client;
            }
        } finally {
            LOCK.unlock();
        }
    }

    static {
        OBJECT_MAPPER.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        OBJECT_MAPPER.registerModule(new JavaTimeModule());
        OBJECT_MAPPER.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    /**
     * 设置缓存过期时间
     *
     * @param key 键
     * @param expireTime 时间
     * @param timeUnit 时间单位
     */
    public static void setExpireTime(String key, long expireTime, TimeUnit timeUnit) {
        if (expireTime > 0) {
            redisTemplate.expire(key, expireTime, timeUnit);
        }
    }

    /**
     * 移除缓存过期时间
     *
     * @param key 键
     */
    public static void removeExpireTime(String key) {
        redisTemplate.boundValueOps(key).persist();
    }

    /**
     * 判断缓存是否存在
     *
     * @param key 键
     * @return 存在 true 不存在 false
     */
    public static boolean hasKey(String key) {
        return Boolean.TRUE.equals(redisTemplate.hasKey(key));
    }

    /**
     * 删除缓存
     *
     * @param keys 键集合
     */
    public static void delete(String... keys) {
        redisTemplate.delete(Arrays.asList(keys));
    }

    /**
     * 放入缓存
     *
     * @param key 键
     * @param value 值
     */
    public static void set(String key, Object value) {
        Assert.notNull(key, "key cannot be null");
        Assert.notNull(value, "value cannot be null");
        redisTemplate.opsForValue().set(key, CommonUtil.serializeObject(value));
    }

    /**
     * 放入缓存
     *
     * @param key 键
     * @param value 值
     * @param time 过期时间
     * @param timeUnit 时间单位
     */
    public static void set(String key, Object value, long time, TimeUnit timeUnit) {
        Assert.notNull(key, "key cannot be null");
        Assert.notNull(value, "value cannot be null");
        redisTemplate.opsForValue().set(key, CommonUtil.serializeObject(value), time, timeUnit);
    }

    /**
     * 获取缓存值
     *
     * @param key 键
     * @param clazz 值类型
     * @return 值
     * @param <T> T
     */
    public static <T> T get(String key, Class<T> clazz) {
        String val = redisTemplate.opsForValue().get(key);
        if (val == null) {
            return null;
        }
        return CommonUtil.deserializeObject(val, clazz);
    }

    /**
     *  获取分布式锁
     *
     * @param key 键
     * @return 分布式锁
     */
    public static RLock getLock(String key) {
        return redissonClient.getLock(key);
    }
}
