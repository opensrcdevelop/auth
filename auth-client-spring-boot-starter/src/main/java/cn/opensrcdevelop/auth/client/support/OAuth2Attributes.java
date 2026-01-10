package cn.opensrcdevelop.auth.client.support;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import org.springframework.util.Assert;

@Getter
public class OAuth2Attributes implements Serializable {

    @Serial
    private static final long serialVersionUID = -1529402053442724890L;

    private final Map<String, Object> attributes = new HashMap<>();

    public OAuth2Attributes(Map<String, Object> userAttributes) {
        Assert.notNull(userAttributes, "UserAttributes can not be null");
        attributes.putAll(userAttributes);
    }

    public void setAttribute(String key, Object value) {
        attributes.put(key, value);
    }

    public void removeAttribute(String key) {
        attributes.remove(key);
    }

    @SuppressWarnings("unchecked")
    public <T> T getAttribute(String key) {
        return (T) attributes.get(key);
    }
}
