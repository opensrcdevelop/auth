package cn.opensrcdevelop.tenant.event;

import org.springframework.context.ApplicationEvent;

import java.io.Serial;

public class RemoveTenantEvent extends ApplicationEvent {

    @Serial
    private static final long serialVersionUID = 6871944720037173744L;

    public RemoveTenantEvent(Object source) {
        super(source);
    }
}
