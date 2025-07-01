package cn.opensrcdevelop.common.util;

import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationEvent;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;

@Component
@SuppressWarnings("unused")
public class SpringContextUtil implements ApplicationContextAware {

    private static ApplicationContext context;

    @Override
    @SuppressWarnings("all")
    public void setApplicationContext(ApplicationContext ctx) throws BeansException {
        SpringContextUtil.context = ctx;
    }

    @SuppressWarnings("unchecked")
    public static <T> T getBean(String name) {
        return (T) context.getBean(name);
    }

    public static <T> T getBean(Class<T> clazz) {
        return context.getBean(clazz);
    }

    public static <T> T getBean(String name, Class<T> clazz) {
        return context.getBean(name, clazz);
    }

    public static String getProperty(String name) {
        return context.getEnvironment().getProperty(name);
    }

    public static String getProperty(String name, String defaultValue) {
        return context.getEnvironment().getProperty(name, defaultValue);
    }

    public static <T> List<T> getBeans(Class<T> clazz) {
        DefaultListableBeanFactory beanFactory = (DefaultListableBeanFactory) context.getAutowireCapableBeanFactory();
        var beans = beanFactory.getBeansOfType(clazz);
        if (MapUtils.isNotEmpty(beans)) {
             return beans.values().stream().toList();
        } else {
            return Collections.emptyList();
        }
    }

    public static void publishEvent(ApplicationEvent event) {
        context.publishEvent(event);
    }

    public static BeanFactory getBeanFactory() {
        return context.getAutowireCapableBeanFactory();
    }
}
