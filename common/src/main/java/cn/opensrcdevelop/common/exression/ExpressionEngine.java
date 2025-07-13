package cn.opensrcdevelop.common.exression;

import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import lombok.Setter;
import org.apache.commons.jexl3.JexlBuilder;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlScript;
import org.apache.commons.jexl3.MapContext;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.stream.Collectors;

@Setter
public class ExpressionEngine {

    private final JexlEngine jexlEngine;

    public ExpressionEngine(int cacheSize) {
        JexlBuilder builder = new JexlBuilder()
                .charset(StandardCharsets.UTF_8)
                .cache(cacheSize)
                .strict(true)
                .silent(false)
                .cancellable(true);

        // 注册自定义函数
        builder.namespaces(CommonUtil.stream(SpringContextUtil.getBeans(ICustomFunction.class))
                .collect(Collectors.toMap(ICustomFunction::getFullNamespace, i -> i)));

        jexlEngine = builder.create();
    }

    /**
     * 执行表达式
     *
     * @param expression 表达式
     * @return 执行结果
     */
    public Object evaluate(String expression, Map<String, Object> context) {
        JexlScript script = jexlEngine.createScript(expression);
        return script.execute(new MapContext(context));
    }

    /**
     * 执行表达式
     *
     * @param expression 表达式
     * @return 执行结果
     */
    public Object evaluate(String expression) {
        JexlScript script = jexlEngine.createScript(expression);
        return script.execute(null);
    }
}
