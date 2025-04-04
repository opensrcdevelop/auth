package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.resource.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupRequestDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupResponseDto;
import cn.opensrcdevelop.auth.biz.entity.ResourceGroup;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface ResourceGroupService extends IService<ResourceGroup> {

    PageData<ResourceGroupResponseDto> list(int page, int size, String keyword);

    PageData<ResourceResponseDto> getGroupResources(int page, int size, String resourceGroupId, String keyword);

    ResourceGroupResponseDto detail(String resourceGroupId);

    void removeResourceGroup(List<String> resourceGroupId);

    void createResourceGroup(ResourceGroupRequestDto requestDto);

    void updateResourceGroup(ResourceGroupRequestDto requestDto);
}
