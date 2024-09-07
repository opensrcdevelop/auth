package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.dto.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceGroupResponseDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceRequestDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.entity.Permission;
import cn.opensrcdevelop.auth.biz.entity.Resource;
import cn.opensrcdevelop.auth.biz.entity.ResourceGroup;
import cn.opensrcdevelop.auth.biz.mapper.ResourceMapper;
import cn.opensrcdevelop.auth.biz.repository.ResourceRepository;
import cn.opensrcdevelop.auth.biz.service.PermissionService;
import cn.opensrcdevelop.auth.biz.service.ResourceGroupService;
import cn.opensrcdevelop.auth.biz.service.ResourceService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class ResourceServiceImpl extends ServiceImpl<ResourceMapper, Resource> implements ResourceService {

    private final ResourceRepository resourceRepository;
    private final PermissionService permissionService;
    private final ResourceGroupService resourceGroupService;

    /**
     * 创建资源
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createResource(ResourceRequestDto requestDto) {
        // 1. 属性设置
        Resource resource = new Resource();
        resource.setResourceName(requestDto.getName());
        resource.setResourceId(CommonUtil.getUUIDString());
        resource.setResourceCode(requestDto.getCode());
        resource.setDescription(requestDto.getDesc());
        resource.setResourceGroupId(requestDto.getResourceGroupId());
        resource.setApiIdentifier(requestDto.getApi());

        // 2. 数据库操作
        super.save(resource);
    }

    /**
     * 获取资源列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 资源名称 / 标识检索关键字
     * @return 资源列表
     */
    @Override
    public PageData<ResourceResponseDto> list(int page, int size, String keyword) {
        // 1. 查询数据库
        Page<Resource> pageRequest = new Page<>(page, size);
        resourceRepository.searchResources(pageRequest, keyword);

        // 2. 属性设置
        PageData<ResourceResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var resources = CommonUtil.stream(pageRequest.getRecords()).map(resource -> {
            ResourceResponseDto resourceResponse = new ResourceResponseDto();
            resourceResponse.setId(resource.getResourceId());
            resourceResponse.setName(resource.getResourceName());
            resourceResponse.setCode(resource.getResourceCode());
            resourceResponse.setApi(resource.getApiIdentifier());

            var resourceGroup = resource.getResourceGroup();
            if (resourceGroup != null) {
                ResourceGroupResponseDto resourceGroupResponse = new ResourceGroupResponseDto();
                resourceGroupResponse.setId(resourceGroup.getResourceGroupId());
                resourceGroupResponse.setName(resourceGroup.getResourceGroupName());
                resourceGroupResponse.setCode(resourceGroup.getResourceGroupCode());

                resourceResponse.setResourceGroup(resourceGroupResponse);
            }
            return resourceResponse;
        }).toList();
        pageData.setList(resources);

        return pageData;
    }

    /**
     * 获取资源详情
     *
     * @param resourceId 资源ID
     * @return 资源详情
     */
    @Override
    public ResourceResponseDto detail(String resourceId) {
        ResourceResponseDto resourceResponse = new ResourceResponseDto();
        // 1. 基本信息
        Resource resource = super.getById(resourceId);
        if (resource == null) {
            return resourceResponse;
        }

        resourceResponse.setId(resource.getResourceId());
        resourceResponse.setName(resource.getResourceName());
        resourceResponse.setCode(resource.getResourceCode());
        resourceResponse.setDesc(resource.getDescription());
        resourceResponse.setApi(resource.getApiIdentifier());

        // 2. 资源组信息
        ResourceGroup resourceGroup = resourceGroupService.getById(resource.getResourceGroupId());
        if (resourceGroup != null) {
            ResourceGroupResponseDto resourceGroupResponse = new ResourceGroupResponseDto();
            resourceGroupResponse.setId(resourceGroup.getResourceGroupId());
            resourceGroupResponse.setName(resourceGroup.getResourceGroupName());
            resourceGroupResponse.setCode(resourceGroup.getResourceGroupCode());

            resourceResponse.setResourceGroup(resourceGroupResponse);
        }
        return resourceResponse;
    }

    /**
     * 获取资源内权限
     *
     * @param page 页数
     * @param size 条数
     * @param resourceId 资源ID
     * @param keyword 资源名称 / 标识检索关键字
     * @return 资源内权限
     */
    @Override
    public PageData<PermissionResponseDto> getResourcePermissions(int page, int size, String resourceId, String keyword) {
        // 1. 查询数据库
        Page<Permission> pageRequest = new Page<>(page, size);
        permissionService.getResourcePermissions(pageRequest, resourceId, keyword);

        // 2. 属性设置
        PageData<PermissionResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var permissions = CommonUtil.stream(pageRequest.getRecords()).map(permission -> {
            PermissionResponseDto permissionResponse = new PermissionResponseDto();
            permissionResponse.setPermissionId(permission.getPermissionId());
            permissionResponse.setPermissionName(permission.getPermissionName());
            permissionResponse.setPermissionCode(permission.getPermissionCode());

            return permissionResponse;
        }).toList();
        pageData.setList(permissions);
        return pageData;
    }

    /**
     * 更新资源
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updateResource(ResourceRequestDto requestDto) {
        // 1. 获取版本号
        var rawResource = super.getById(requestDto.getId());
        if (Objects.isNull(rawResource)) {
            return;
        }

        // 2. 属性设置
        Resource updateResource = new Resource();
        updateResource.setResourceId(requestDto.getId());
        updateResource.setResourceName(requestDto.getName());
        updateResource.setResourceCode(requestDto.getCode());
        updateResource.setApiIdentifier(requestDto.getApi());
        updateResource.setDescription(requestDto.getDesc());
        updateResource.setVersion(rawResource.getVersion());

        // 3. 数据库操作
        super.updateById(updateResource);
    }

    /**
     * 删除资源
     *
     * @param resourceIds 资源ID集合
     */
    @Transactional
    @Override
    public void removeResource(List<String> resourceIds) {
        // 1. 删除资源
        super.remove(Wrappers.<Resource>lambdaQuery().in(Resource::getResourceId, resourceIds));

        // 2. 删除资源下的所有权限
        permissionService.removeResourcePermissions(resourceIds);
    }
}
