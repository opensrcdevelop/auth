package cn.opensrcdevelop.auth.biz.service.resource.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceRequestDto;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.auth.biz.entity.resource.group.ResourceGroup;
import cn.opensrcdevelop.auth.biz.mapper.resource.ResourceMapper;
import cn.opensrcdevelop.auth.biz.repository.resource.ResourceRepository;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.biz.service.resource.ResourceService;
import cn.opensrcdevelop.auth.biz.service.resource.group.ResourceGroupService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class ResourceServiceImpl extends ServiceImpl<ResourceMapper, Resource> implements ResourceService {

    private final ResourceRepository resourceRepository;

    @jakarta.annotation.Resource
    private ResourceGroupService resourceGroupService;

    @jakarta.annotation.Resource
    private PermissionService permissionService;

    /**
     * 创建资源
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.RESOURCE,
            sysOperation = SysOperationType.CREATE,
            success = "创建了资源（{{ @linkGen.toLink(#resourceId, T(ResourceType).RESOURCE) }}）",
            fail = "创建资源（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createResource(ResourceRequestDto requestDto) {
        // 1. 检查资源标识是否存在
        checkResourceCode(requestDto, null);

        // 2. 属性设置
        String resourceId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("resourceId", resourceId);

        Resource resource = new Resource();
        resource.setResourceName(requestDto.getName());
        resource.setResourceId(resourceId);
        resource.setResourceCode(requestDto.getCode());
        resource.setDescription(requestDto.getDesc());
        resource.setResourceGroupId(requestDto.getResourceGroupId());
        resource.setApiIdentifier(requestDto.getApi());

        // 3. 数据库操作
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
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.RESOURCE,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了资源（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).RESOURCE) }}）",
            fail = "修改资源（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).RESOURCE) }}）失败"
    )
    @Transactional
    @Override
    public void updateResource(ResourceRequestDto requestDto) {
        String resourceId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawResource = super.getById(requestDto.getId());
        if (Objects.isNull(rawResource)) {
            return;
        }
        compareObjBuilder.id(resourceId);
        compareObjBuilder.before(rawResource);

        // 2. 检查资源标识是否存在
        checkResourceCode(requestDto, rawResource);

        // 3. 属性设置
        Resource updateResource = new Resource();
        updateResource.setResourceId(requestDto.getId());
        updateResource.setResourceName(requestDto.getName());
        updateResource.setResourceCode(requestDto.getCode());
        updateResource.setApiIdentifier(requestDto.getApi());
        updateResource.setDescription(requestDto.getDesc());
        updateResource.setVersion(rawResource.getVersion());

        // 4. 数据库操作
        super.updateById(updateResource);

        compareObjBuilder.after(super.getById(resourceId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 删除资源
     *
     * @param resourceIds 资源ID集合
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.RESOURCE,
            sysOperation = SysOperationType.DELETE,
            success = "删除了资源（{{ @linkGen.toLinks(#resourceIds, T(ResourceType).RESOURCE) }}）",
            fail = "删除资源（{{ @linkGen.toLinks(#resourceIds, T(ResourceType).RESOURCE) }}）失败"
    )
    @Transactional
    @Override
    public void removeResource(List<String> resourceIds) {
        // 1. 删除资源
        super.remove(Wrappers.<Resource>lambdaQuery().in(Resource::getResourceId, resourceIds));

        // 2. 删除资源下的所有权限
        permissionService.removeResourcePermissions(resourceIds);
    }

    private void checkResourceCode(ResourceRequestDto requestDto, Resource rawResource) {
        if (Objects.nonNull(rawResource) && StringUtils.equals(requestDto.getCode(), rawResource.getResourceCode())) {
            return;
        }

        if (Objects.isNull(resourceGroupService.getById(requestDto.getResourceGroupId()))) {
            throw new BizException(MessageConstants.RESOURCE_MSG_1001);
        }

        if (Objects.nonNull(super.getOne(Wrappers.<Resource>lambdaQuery().eq(Resource::getResourceCode, requestDto.getCode()).and(q -> q.eq(Resource::getResourceGroupId, requestDto.getResourceGroupId()))))) {
            throw new BizException(MessageConstants.RESOURCE_MSG_1000, requestDto.getCode());
        }
    }
}
