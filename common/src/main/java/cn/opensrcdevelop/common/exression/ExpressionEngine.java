package cn.opensrcdevelop.common.exression;

import cn.opensrcdevelop.common.util.CommonUtil;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.Setter;
import org.apache.commons.jexl3.JexlBuilder;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlScript;
import org.apache.commons.jexl3.MapContext;
import org.apache.commons.jexl3.introspection.JexlPermissions;

@Setter
public class ExpressionEngine {

    private final JexlEngine jexlEngine;

    public ExpressionEngine(int cacheSize, List<ICustomFunction> customFunctionList) {
        JexlBuilder builder = new JexlBuilder()
                .charset(StandardCharsets.UTF_8)
                .cache(cacheSize)
                .strict(true)
                .silent(false);

        // 添加自定义函数的权限
        JexlPermissions jexlPermissions = new JexlPermissions.ClassPermissions(CommonUtil
                .stream(customFunctionList).map(ICustomFunction::getClass).toList().toArray(new Class[0]));
        builder.permissions(jexlPermissions);

        // 注册自定义函数
        builder.namespaces(CommonUtil.stream(customFunctionList)
                .collect(Collectors.toMap(ICustomFunction::getFullNamespace, i -> i)));

        jexlEngine = builder.create();
    }

    /**
     * 执行表达式
     *
     * @param expression
     *            表达式
     * @return 执行结果
     */
    public Object evaluate(String expression, Map<String, Object> context) {
        JexlScript script = jexlEngine.createScript(expression);
        return script.execute(new MapContext(context));
    }

    /**
     * 执行表达式
     *
     * @param expression
     *            表达式
     * @return 执行结果
     */
    public Object evaluate(String expression) {
        JexlScript script = jexlEngine.createScript(expression);
        return script.execute(null);
    }
}
