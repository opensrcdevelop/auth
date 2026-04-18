package cn.opensrcdevelop.auth.biz.service.permission.request;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestItemResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequest;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface PermissionRequestService extends IService<PermissionRequest> {

    void submitRequest(PermissionRequestCreateDto dto);

    PageData<PermissionRequestResponseDto> listRequests(List<String> userId, String usernameSearchKeyword, int page,
            int size);

    List<PermissionRequestItemResponseDto> listRequestItems(String userId, String requestId);

    void cancelRequest(String requestId);
}
