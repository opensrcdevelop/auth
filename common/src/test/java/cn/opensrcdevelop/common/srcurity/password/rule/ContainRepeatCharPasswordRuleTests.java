package cn.opensrcdevelop.common.srcurity.password.rule;

import cn.opensrcdevelop.common.security.password.rule.ContainRepeatCharPasswordRule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ContainRepeatCharPasswordRuleTests {

    private final ContainRepeatCharPasswordRule rule = new ContainRepeatCharPasswordRule(4);

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
                Arguments.of("ab111", true),
                Arguments.of("111212311111", false)
        );
    }


    @Test
    void testGetRuleName() {
        assertEquals("禁止包含 4 位及以上连续重复字符。", rule.getRuleName());
    }
}
