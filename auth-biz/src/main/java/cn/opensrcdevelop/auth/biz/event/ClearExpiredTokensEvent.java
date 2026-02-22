package cn.opensrcdevelop.auth.biz.event;

import java.io.Serial;
import org.springframework.context.ApplicationEvent;

public class ClearExpiredTokensEvent extends ApplicationEvent {

    @Serial
    private static final long serialVersionUID = -7564529850029981988L;

    public ClearExpiredTokensEvent(Object source) {
        super(source);
    }
}
