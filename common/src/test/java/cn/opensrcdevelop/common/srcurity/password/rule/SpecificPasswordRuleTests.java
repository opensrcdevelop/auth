package cn.opensrcdevelop.common.srcurity.password.rule;

import static org.junit.jupiter.api.Assertions.*;

import cn.opensrcdevelop.common.security.password.rule.SpecificPasswordRule;
import java.util.Arrays;
import java.util.Collections;
import org.junit.jupiter.api.Test;

class SpecificPasswordRuleTests {

    @Test
    void testValidate_EmptyPassword() {
        SpecificPasswordRule rule = new SpecificPasswordRule(Arrays.asList("password123", "admin123"));
        assertFalse(rule.validate(""));
    }

    @Test
    void testValidate_PasswordNotInSpecificList() {
        SpecificPasswordRule rule = new SpecificPasswordRule(Arrays.asList("password123", "admin123"));
        assertTrue(rule.validate("securePassword"));
    }

    @Test
    void testValidate_PasswordInSpecificList() {
        SpecificPasswordRule rule = new SpecificPasswordRule(Arrays.asList("password123", "admin123"));
        assertFalse(rule.validate("password123"));
    }

    @Test
    void testValidate_EmptySpecificList() {
        SpecificPasswordRule rule = new SpecificPasswordRule(Collections.emptyList());
        assertTrue(rule.validate("anyPassword"));
    }

    @Test
    void testGetRuleName() {
        SpecificPasswordRule rule = new SpecificPasswordRule(Arrays.asList("password123", "admin123"));
        assertEquals("禁止使用特定密码。", rule.getRuleName());
    }
}
