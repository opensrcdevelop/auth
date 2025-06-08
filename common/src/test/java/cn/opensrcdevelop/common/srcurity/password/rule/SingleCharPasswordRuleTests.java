package cn.opensrcdevelop.common.srcurity.password.rule;

import cn.opensrcdevelop.common.security.password.rule.SingleCharPasswordRule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SingleCharPasswordRuleTests {

    private final SingleCharPasswordRule rule = new SingleCharPasswordRule();

    @ParameterizedTest
    @MethodSource("provideTestCases")
    void testValidate(String password, boolean expected) {
        assertEquals(expected, rule.validate(password));
    }

    private static Stream<Arguments> provideTestCases() {
        return Stream.of(
                Arguments.of(null, false),
                Arguments.of("", false),
                Arguments.of("a", false),
                Arguments.of("11111", false),
                Arguments.of("123455", true)
        );
    }

    @Test
    void testGetRuleName() {
        assertEquals("禁止全部单一字符。", rule.getRuleName());
    }
}
