package cn.opensrcdevelop.common.expression;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import cn.opensrcdevelop.common.exression.ExpressionEngine;
import cn.opensrcdevelop.common.exression.function.TimeFunction;
import java.util.List;
import org.junit.jupiter.api.Test;

class ExpressionEngineTests {

    private final ExpressionEngine expressionEngine = new ExpressionEngine(1024, List.of(new TimeFunction()));

    @Test
    void evaluateTest1() {
        String expression = "fn_time:nowTime()";
        String result = (String) expressionEngine.evaluate(expression);
        assertNotNull(result);
    }

    @Test
    void evaluateTest2() {
        String expression = "System.exit(0)";
        assertNull(expressionEngine.evaluate(expression));
    }
}
