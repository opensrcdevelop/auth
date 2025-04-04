package cn.opensrcdevelop.common.srcurity.password.rule;


import cn.opensrcdevelop.common.security.password.rule.ContainConsecutiveCharPasswordRule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ContainConsecutiveCharPasswordRuleTests {

    private final ContainConsecutiveCharPasswordRule rule = new ContainConsecutiveCharPasswordRule(4);

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
                Arguments.of("abc", true),
                Arguments.of("cba", true),
                Arguments.of("acb", true),
                Arguments.of("123", true),
                Arguments.of("123456", false),
                Arguments.of("1234", false),
                Arguments.of("4321", false),
                Arguments.of("abc1234", false),
                Arguments.of("ab123abcdef", false),
                Arguments.of("ad123acd", true),
                Arguments.of("1234abcz", false)
        );
    }


    @Test
    void testGetRuleName() {
        assertEquals("禁止包含 4 位及以上连续字符。", rule.getRuleName());
    }
}
