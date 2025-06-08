package cn.opensrcdevelop.common.srcurity.password.rule;

import cn.opensrcdevelop.common.security.password.rule.UserInfoPasswordRule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;

class UserInfoPasswordRuleTests {

    private UserInfoPasswordRule rule;

    @BeforeEach
    void setUp() {
        // 初始化测试环境
    }

    @Test
    void testValidate_EmptyPassword() {
        rule = new UserInfoPasswordRule(Arrays.asList("user", "email"));
        assertFalse(rule.validate(""));
    }

    @Test
    void testValidate_PasswordContainsUserInfo() {
        rule = new UserInfoPasswordRule(Arrays.asList("user", "email"));
        assertFalse(rule.validate("user123"));
    }

    @Test
    void testValidate_PasswordDoesNotContainUserInfo() {
        rule = new UserInfoPasswordRule(Arrays.asList("user", "email"));
        assertTrue(rule.validate("securePassword"));
    }

    @Test
    void testValidate_EmptyUserInfoList() {
        rule = new UserInfoPasswordRule(Collections.emptyList());
        assertTrue(rule.validate("anyPassword"));
    }

    @Test
    void testGetRuleName() {
        rule = new UserInfoPasswordRule(Arrays.asList("user", "email"));
        assertEquals("禁止包含用户信息。", rule.getRuleName());
    }
}
