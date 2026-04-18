package cn.opensrcdevelop.auth.biz.repository.permission;

import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequestItem;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

public interface PermissionRequestRepository {

    boolean hasActivePendingRequest(String userId, List<String> permissionIds);

    void searchPermissionRequests(IPage<PermissionRequest> page, List<String> userIds, String usernameSearchKeyword);

    List<PermissionRequestItem> getPermissionRequestItems(String userId, String requestId);
}
