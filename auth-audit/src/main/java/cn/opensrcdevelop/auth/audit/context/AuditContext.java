package cn.opensrcdevelop.auth.audit.context;

import com.alibaba.ttl.TransmittableThreadLocal;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;

import java.util.*;

public class AuditContext {

    /** 审计内容 */
    private static final TransmittableThreadLocal<String> CONTENT = new TransmittableThreadLocal<>();

    /** 目标对象 */
    private static final TransmittableThreadLocal<List<Object>> TARGET_OBJ_LIST = new TransmittableThreadLocal<>();

    /** 额外数据 */
    private static final TransmittableThreadLocal<Map<String, Object>> ADDITIONAL_DATA = new TransmittableThreadLocal<>();

    public static String getContent() {
        return CONTENT.get();
    }

    public static void setContent(String content) {
        CONTENT.set(content);
    }

    public static void removeContent() {
        CONTENT.remove();
    }

    public static Object getAdditionalData(String key) {
        return ADDITIONAL_DATA.get().get(key);
    }

    public static Map<String, Object> getAdditionalData() {
        Map<String, Object> data = ADDITIONAL_DATA.get();
        if (MapUtils.isEmpty(data)) {
            ADDITIONAL_DATA.set(new HashMap<>());
        }
        return data;
    }

    public static void putAdditionalData(String key, Object value) {
        Map<String, Object> data = ADDITIONAL_DATA.get();
        if (MapUtils.isEmpty(data)) {
            Map<String, Object> map = new HashMap<>();
            map.put(key, value);
            ADDITIONAL_DATA.set(map);
            return;
        }
        data.put(key, value);
    }

    public static void putAdditionalData(Map<String, Object> value) {
        ADDITIONAL_DATA.set(value);
    }

    public static void removeAdditionalData() {
        ADDITIONAL_DATA.remove();
    }

    public static List<Object> getTargetObj() {
        return TARGET_OBJ_LIST.get();
    }

    public static void setTargetObj(List<Object> targetObjList) {
        if (CollectionUtils.isNotEmpty(targetObjList)) {
            TARGET_OBJ_LIST.set(targetObjList);
        }
    }

    public static void addTargetObj(Object targetObj) {
        if (Objects.nonNull(targetObj)) {
            List<Object> targetObjList = TARGET_OBJ_LIST.get();
            if (CollectionUtils.isEmpty(targetObjList)) {
                targetObjList = new ArrayList<>();
            }
            targetObjList.add(targetObj);
            TARGET_OBJ_LIST.set(targetObjList);
        }
    }

    public static void removeTargetObj() {
        TARGET_OBJ_LIST.remove();
    }

    public static void removeAuditContext() {
        removeContent();
        removeTargetObj();
        removeAdditionalData();
    }
}
