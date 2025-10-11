package cn.opensrcdevelop.auth.audit.context;

import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import com.alibaba.ttl.TransmittableThreadLocal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unused")
public class AuditContext {

    private AuditContext() {
    }

    /**
     * 操作对象列表
     */
    private static final TransmittableThreadLocal<List<CompareObj<?>>> COMPARE_OBJ_LIST = new TransmittableThreadLocal<>() {

        @Override
        protected List<CompareObj<?>> initialValue() {
            return new ArrayList<>();
        }
    };

    /**
     * SPEL 变量
     */
    private static final TransmittableThreadLocal<Map<String, Object>> SPEL_VARIABLES = new TransmittableThreadLocal<>() {

        @Override
        protected Map<String, Object> initialValue() {
            return new HashMap<>();
        }
    };

    public static void addCompareObj(CompareObj<?> operationObj) {
        COMPARE_OBJ_LIST.get().add(operationObj);
    }

    public static void clearCompareObjList() {
        COMPARE_OBJ_LIST.get().clear();
    }

    public static List<CompareObj<?>> getCompareObjList() {
        return COMPARE_OBJ_LIST.get();
    }

    public static void setSpelVariable(String key, Object value) {
        SPEL_VARIABLES.get().put(key, value);
    }

    public static Map<String, Object> getSpelVariables() {
        return SPEL_VARIABLES.get();
    }

    public static void clearAuditContext() {
        COMPARE_OBJ_LIST.remove();
        SPEL_VARIABLES.remove();
    }
}
