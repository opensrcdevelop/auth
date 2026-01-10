package cn.opensrcdevelop.auth.client.support;

import java.util.Map;

public interface ICheckAttribute {

    boolean check(Map<String, Object> attributes);
}
