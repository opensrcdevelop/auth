package cn.opensrcdevelop.common.exression;

public interface ICustomFunction {

    String FUNCTION_PREFIX = "fn_";

    String getNamespace();

    default String getFullNamespace() {
        return FUNCTION_PREFIX.concat(getNamespace());
    }
}
