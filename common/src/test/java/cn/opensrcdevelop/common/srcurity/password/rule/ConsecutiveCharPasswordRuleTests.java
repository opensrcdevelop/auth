package cn.opensrcdevelop.common.srcurity.password.rule;

import cn.opensrcdevelop.common.security.password.rule.ConsecutiveCharPasswordRule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ConsecutiveCharPasswordRuleTests {

    private final ConsecutiveCharPasswordRule rule = new ConsecutiveCharPasswordRule();

    @ParameterizedTest
    @MethodSource("provideTestCases")
    void testValidate(String password, boolean expected) {
        assertEquals(expected, rule.validate(password));
    }

    private static Stream<Arguments> provideTestCases() {
        return Stream.of(
                Arguments.of(null, false),
                Arguments.of("", false),
                Arguments.of("a", true),
                Arguments.of("abc", false),
                Arguments.of("cba", false),
                Arguments.of("acb", true),
                Arguments.of("123456", false),
                Arguments.of("123456abc", true)
        );
    }

    @Test
    void testGetRuleName() {
        assertEquals("禁止全部连续字符。", rule.getRuleName());
    }
}
