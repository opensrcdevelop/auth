package cn.opensrcdevelop.auth.audit.compare;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class PropertyValueChange {

    private String propertyName;

    private Object left;

    private Object right;
}
