package cn.opensrcdevelop.auth.biz.service.permission.request;

/**
 * 自动批准配置查询服务 用于判断指定权限是否开启了自动批准功能
 */
public interface PermissionAutoApproveService {

    /**
     * 判断指定权限是否开启了自动批准
     *
     * @param permissionId
     *            权限ID
     * @return true 表示已开启自动批准
     */
    boolean isEnabled(String permissionId);
}
