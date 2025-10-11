package cn.opensrcdevelop.common.exression.function;

import cn.opensrcdevelop.common.exression.ICustomFunction;
import org.springframework.stereotype.Component;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

@Component
public class TimeFunction implements ICustomFunction {

    public String nowTime() {
        LocalTime nowTime = LocalTime.now();
        return nowTime.format(DateTimeFormatter.ofPattern("HHmm"));
    }

    @Override
    public String getNamespace() {
        return "time";
    }
}
