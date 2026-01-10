package cn.opensrcdevelop.auth.biz.service.resource;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceRequestDto;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

public interface ResourceService extends IService<Resource> {

    void createResource(ResourceRequestDto requestDto);

    PageData<ResourceResponseDto> list(int page, int size, String keyword);

    ResourceResponseDto detail(String resourceId);

    PageData<PermissionResponseDto> getResourcePermissions(int page, int size, String resourceId, String keyword);

    void updateResource(ResourceRequestDto requestDto);

    void removeResource(List<String> resourceId);
}
