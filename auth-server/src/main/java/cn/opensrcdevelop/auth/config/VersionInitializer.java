package cn.opensrcdevelop.auth.config;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * 版本初始化组件.
 * <p>
 * 应用启动时从系统属性读取版本信息，并设置到 MDC 中，以便在日志中输出.
 * </p>
 */
@Slf4j
@Component
public class VersionInitializer implements ApplicationRunner {

    private static final String VERSION_KEY = "version";

    @Value("${app.version:Unknown}")
    private String version;

    @Override
    public void run(ApplicationArguments args) {
        MDC.put(VERSION_KEY, version);
        log.info("应用启动成功，版本: {}", version);
    }
}
