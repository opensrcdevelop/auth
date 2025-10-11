package cn.opensrcdevelop.auth.config;

import lombok.RequiredArgsConstructor;
import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.ClusterServersConfig;
import org.redisson.config.Config;
import org.redisson.config.SingleServerConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.data.redis.RedisProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.serializer.JdkSerializationRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import java.util.Objects;
import java.util.Optional;

@Configuration
@RequiredArgsConstructor
@EnableCaching
public class RedisCacheConfig {

    private final RedisProperties redisProperties;

    @Bean
    public RedisCacheConfiguration redisCacheConfiguration() {
        // 配置键值序列化方式
        return RedisCacheConfiguration.defaultCacheConfig()
                .disableCachingNullValues()
                .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(new StringRedisSerializer()))
                .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(new JdkSerializationRedisSerializer()));
    }

    @Bean
    public RedisCacheWriter redisCacheWriter(RedisConnectionFactory redisConnectionFactory) {
        return RedisCacheWriter.nonLockingRedisCacheWriter(redisConnectionFactory);
    }

    @Bean
    @ConditionalOnProperty(name = {"host","port"}, prefix = "spring.data.redis")
    public RedissonClient  redissonClient() {
        Config config = new Config();
        if (Objects.nonNull(redisProperties.getCluster())) {
            ClusterServersConfig clusterServersConfig = config.useClusterServers();
            redisProperties.getCluster().getNodes().stream().map(node -> "redis://" + node).forEach(clusterServersConfig::addNodeAddress);
            clusterServersConfig.setPassword(redisProperties.getPassword());
            Optional.ofNullable(redisProperties.getTimeout()).ifPresent(t -> clusterServersConfig.setTimeout((int) redisProperties.getTimeout().toSeconds()));
            Optional.ofNullable(redisProperties.getConnectTimeout()).ifPresent(t -> clusterServersConfig.setConnectTimeout((int) redisProperties.getConnectTimeout().toMillis()));
        } else {
            SingleServerConfig singleServerConfig = config.useSingleServer();
            singleServerConfig.setAddress("redis://" + redisProperties.getHost() + ":" + redisProperties.getPort());
            singleServerConfig.setPassword(redisProperties.getPassword());
            Optional.ofNullable(redisProperties.getTimeout()).ifPresent(t -> singleServerConfig.setTimeout((int) redisProperties.getTimeout().toSeconds()));
            Optional.ofNullable(redisProperties.getConnectTimeout()).ifPresent(t -> singleServerConfig.setConnectTimeout((int) redisProperties.getConnectTimeout().toMillis()));
        }

        return Redisson.create(config);
    }
}
