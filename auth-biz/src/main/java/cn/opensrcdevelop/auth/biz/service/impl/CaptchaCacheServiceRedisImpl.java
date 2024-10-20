package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.common.util.RedisUtil;
import com.anji.captcha.service.CaptchaCacheService;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

@Service
public class CaptchaCacheServiceRedisImpl implements CaptchaCacheService {

    @Override
    public void set(String key, String value, long expiresInSeconds) {
        RedisUtil.set(key, value, expiresInSeconds, TimeUnit.SECONDS);
    }

    @Override
    public boolean exists(String key) {
        return RedisUtil.hasKey(key);
    }

    @Override
    public void delete(String key) {
        RedisUtil.delete(key);
    }

    @Override
    public String get(String key) {
        return RedisUtil.get(key, String.class);
    }

    @Override
    public String type() {
        return "redis";
    }
}
