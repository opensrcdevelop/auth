package cn.opensrcdevelop.auth.audit.event;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.audit.entity.ObjChangeLog;
import java.io.Serial;
import java.util.List;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

@Getter
public class AuditEvent extends ApplicationEvent {

    @Serial
    private static final long serialVersionUID = -228766484238594273L;

    private final AuditLog auditLog;

    private final List<ObjChangeLog> objChangeLogs;

    public AuditEvent(AuditLog auditLog, List<ObjChangeLog> objChangeLogs) {
        super(auditLog);
        this.auditLog = auditLog;
        this.objChangeLogs = objChangeLogs;
    }
}
