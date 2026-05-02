package cn.opensrcdevelop.auth.client.authorize;

import lombok.Getter;
import lombok.Setter;
import org.aopalliance.intercept.MethodInvocation;
import org.jspecify.annotations.Nullable;
import org.springframework.security.access.expression.SecurityExpressionRoot;
import org.springframework.security.access.expression.method.MethodSecurityExpressionOperations;
import org.springframework.security.core.Authentication;

import java.util.function.Supplier;

public class AuthorizeExpressionRootObject extends SecurityExpressionRoot<MethodInvocation>
        implements
            MethodSecurityExpressionOperations {

    private Object filterObject;
    private Object returnObject;
    private Object target;

    @Getter
    @Setter
    private MethodInvocation methodInvocation;

    public AuthorizeExpressionRootObject(Authentication authentication) {
        super(() -> authentication, null);
    }

    public AuthorizeExpressionRootObject(Supplier<? extends @Nullable Authentication> authentication) {
        super(authentication, null);
    }

    @Override
    public void setFilterObject(@Nullable Object filterObject) {
        this.filterObject = filterObject;
    }

    @Override
    public Object getFilterObject() {
        return filterObject;
    }

    @Override
    public void setReturnObject(Object returnObject) {
        this.returnObject = returnObject;
    }

    @Override
    public Object getReturnObject() {
        return returnObject;
    }

    @Override
    public Object getThis() {
        return target;
    }

    public void setThis(Object target) {
        this.target = target;
    }
}
