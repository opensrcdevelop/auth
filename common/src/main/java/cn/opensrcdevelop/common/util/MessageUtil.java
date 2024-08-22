package cn.opensrcdevelop.common.util;

import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import java.util.List;

@Component
@RequiredArgsConstructor
@SuppressWarnings("unused")
public class MessageUtil {

    private final List<MessageSource> messageSources;

    public String getMsg(String code, Object... args) {
        if (CollectionUtils.isEmpty(messageSources)) {
            throw new UnsupportedOperationException("no message source found");
        }

        for(MessageSource messageSource : messageSources) {
            try {
                return messageSource.getMessage(code, args, LocaleContextHolder.getLocale());
            } catch (NoSuchMessageException ignored) {
                // 继续获取 message
            }
        }
        return null;
    }

    public String getMsg(String code, String defaultMsg, Object... args) {
        String msg = null;
        if (CollectionUtils.isEmpty(messageSources)) {
            throw new UnsupportedOperationException("no message source found");
        }

        for(MessageSource messageSource : messageSources) {
            if ((msg = messageSource.getMessage(code, args, defaultMsg, LocaleContextHolder.getLocale())) != null) {
                return msg;
            }
        }
        return null;
    }

    public void addMessageSource(MessageSource messageSource) {
        Assert.notNull(messageSource, "messageSource must not be null");
        messageSources.add(messageSource);
    }
}
