package cn.opensrcdevelop.common.srcurity.password.rule;

import static org.junit.jupiter.api.Assertions.assertEquals;

import cn.opensrcdevelop.common.security.password.rule.LengthPasswordRule;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class LengthPasswordRuleTests {

    private final LengthPasswordRule rule = new LengthPasswordRule(8, 16);

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
                Arguments.of("1234567", false),
                Arguments.of("12345678", true),
                Arguments.of("123456789", true),
                Arguments.of("1234567812345678", true),
                Arguments.of("12345678123456781", false));
    }

    @Test
    void testGetRuleName() {
        assertEquals("长度要求 8 位及以上，16 位及以下。", rule.getRuleName());
    }
}
