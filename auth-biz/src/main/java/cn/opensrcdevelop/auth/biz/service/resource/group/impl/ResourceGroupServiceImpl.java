package cn.opensrcdevelop.auth.biz.service.resource.group.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.dto.resource.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupRequestDto;
import cn.opensrcdevelop.auth.biz.dto.resource.group.ResourceGroupResponseDto;
import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.auth.biz.entity.resource.group.ResourceGroup;
import cn.opensrcdevelop.auth.biz.mapper.resource.group.ResourceGroupMapper;
import cn.opensrcdevelop.auth.biz.service.resource.ResourceService;
import cn.opensrcdevelop.auth.biz.service.resource.group.ResourceGroupService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class ResourceGroupServiceImpl extends ServiceImpl<ResourceGroupMapper, ResourceGroup> implements ResourceGroupService {

    private final SystemSettingService systemSettingService;

    @jakarta.annotation.Resource
    @Lazy
    private ResourceService resourceService;


    /**
     * 获取资源组列表
     *
     * @param page    页数
     * @param size    条数
     * @param keyword 资源组名称 / 标识检索关键字
     * @return 资源组列表
     */
    @Override
    public PageData<ResourceGroupResponseDto> list(int page, int size, String keyword) {
        // 1. 查询数据库
        Page<ResourceGroup> pageRequest = new Page<>(page, size);
        List<ResourceGroup> queryResult;
        if (StringUtils.isNotEmpty(keyword)) {
            queryResult = super.list(pageRequest, Wrappers.<ResourceGroup>lambdaQuery().like(ResourceGroup::getResourceGroupName, keyword).or(o -> o.like(ResourceGroup::getResourceGroupCode, keyword)).orderByAsc(ResourceGroup::getResourceGroupCode));
        } else {
            queryResult = super.list(pageRequest, Wrappers.<ResourceGroup>lambdaQuery().orderByAsc(ResourceGroup::getResourceGroupCode));
        }
        // 2. 属性设置
        PageData<ResourceGroupResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var resourceGroups = CommonUtil.stream(queryResult).map(resourceGroup -> {
            ResourceGroupResponseDto resourceGroupResponse = new ResourceGroupResponseDto();
            resourceGroupResponse.setId(resourceGroup.getResourceGroupId());
            resourceGroupResponse.setName(resourceGroup.getResourceGroupName());
            resourceGroupResponse.setCode(resourceGroup.getResourceGroupCode());

            return resourceGroupResponse;
        }).toList();
        pageData.setList(resourceGroups);

        return pageData;
    }

    /**
     * 获取组内资源
     *
     * @param page            页数
     * @param size            条数
     * @param resourceGroupId 资源组ID
     * @param keyword         资源名称 / 标识关键字
     * @return 组内资源
     */
    @Override
    public PageData<ResourceResponseDto> getGroupResources(int page, int size, String resourceGroupId, String keyword) {
        // 1. 查询数据库
        Page<Resource> pageRequest = new Page<>(page, size);
        var query = Wrappers.<Resource>lambdaQuery().eq(Resource::getResourceGroupId, resourceGroupId).orderByAsc(Resource::getResourceCode);
        if (StringUtils.isNotEmpty(keyword)) {
            query.like(Resource::getResourceName, keyword).or(o -> o.like(Resource::getResourceCode, keyword));
        }
        var resources = resourceService.list(pageRequest, query);

        // 2. 属性设置
        PageData<ResourceResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var resourceReps = CommonUtil.stream(resources).map(resource -> {
            ResourceResponseDto resourceResponse = new ResourceResponseDto();
            resourceResponse.setId(resource.getResourceId());
            resourceResponse.setName(resource.getResourceName());
            resourceResponse.setCode(resource.getResourceCode());
            resourceResponse.setApi(resource.getApiIdentifier());
            return resourceResponse;
        }).toList();
        pageData.setList(resourceReps);
        return pageData;
    }

    /**
     * 获取资源组详情
     *
     * @param resourceGroupId 资源组ID
     * @return 资源组详情
     */
    @Override
    public ResourceGroupResponseDto detail(String resourceGroupId) {
        ResourceGroupResponseDto resourceGroupResponse = new ResourceGroupResponseDto();
        // 1. 查询数据库
        ResourceGroup resourceGroup = super.getById(resourceGroupId);

        // 2. 属性设置
        resourceGroupResponse.setId(resourceGroup.getResourceGroupId());
        resourceGroupResponse.setName(resourceGroup.getResourceGroupName());
        resourceGroupResponse.setCode(resourceGroup.getResourceGroupCode());
        resourceGroupResponse.setDesc(resourceGroup.getDescription());
        return resourceGroupResponse;
    }

    /**
     * 删除资源组
     *
     * @param resourceGroupId 资源组ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.RESOURCE_GROUP,
            sysOperation = SysOperationType.DELETE,
            success = "删除了资源组（{{ @linkGen.toLinks(#resourceGroupId, ResourceType.RESOURCE_GROUP) }}）",
            fail = "删除资源组（{{ @linkGen.toLinks(#resourceGroupId, ResourceType.RESOURCE_GROUP) }}）失败"
    )
    @Transactional
    @Override
    public void removeResourceGroup(String resourceGroupId) {
        ResourceGroup resourceGroup = super.getById(resourceGroupId);
        if (Objects.isNull(resourceGroup)) {
            return;
        }
        // 1. 检查是否为系统资源组
        checkIsSystemResourceGroup(resourceGroup.getResourceGroupCode());

        // 2. 删除资源组
        super.removeById(resourceGroupId);

        // 3. 删除组内资源
        var resources = resourceService.list(Wrappers.<Resource>lambdaQuery().eq(Resource::getResourceGroupId, resourceGroupId));
        if (CollectionUtils.isNotEmpty(resources)) {
            resourceService.removeResource(resources.stream().map(Resource::getResourceId).toList());
        }
    }

    /**
     * 创建资源组
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.RESOURCE_GROUP,
            sysOperation = SysOperationType.CREATE,
            success = "创建了资源组（{{ @linkGen.toLink(#resourceGroupId, ResourceType.RESOURCE_GROUP) }}）",
            fail = "创建资源组（{{ @linkGen.toLink(#resourceGroupId, ResourceType.RESOURCE_GROUP) }}）失败"
    )
    @Transactional
    @Override
    public void createResourceGroup(ResourceGroupRequestDto requestDto) {
        // 1. 检查资源组标识是否存在
        checkResourceGroupCode(requestDto, null);

        // 2. 属性编辑
        String resourceGroupId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("resourceGroupId", resourceGroupId);

        ResourceGroup resourceGroup = new ResourceGroup();
        resourceGroup.setResourceGroupId(resourceGroupId);
        resourceGroup.setResourceGroupName(requestDto.getName());
        resourceGroup.setResourceGroupCode(requestDto.getCode());
        resourceGroup.setDescription(requestDto.getDesc());

        // 3. 数据库操作
        super.save(resourceGroup);
    }

    /**
     * 更新资源组
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.RESOURCE_GROUP,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了资源组（{{ @linkGen.toLink(#requestDto.id, ResourceType.RESOURCE_GROUP) }}）",
            fail = "修改资源组（{{ @linkGen.toLink(#requestDto.id, ResourceType.RESOURCE_GROUP) }}）失败"
    )
    @Transactional
    @Override
    public void updateResourceGroup(ResourceGroupRequestDto requestDto) {
        String resourceGroupId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawResourceGroup = super.getById(requestDto.getId());
        if (Objects.isNull(rawResourceGroup)) {
            return;
        }
        compareObjBuilder.id(resourceGroupId);
        compareObjBuilder.before(rawResourceGroup);

        // 2. 检查是否为系统资源组
        checkIsSystemResourceGroup(rawResourceGroup.getResourceGroupCode());

        // 3. 检查资源组标识是否存在
        checkResourceGroupCode(requestDto, rawResourceGroup);

        // 4. 属性编辑
        ResourceGroup updateResourceGroup = new ResourceGroup();
        updateResourceGroup.setResourceGroupId(requestDto.getId());
        updateResourceGroup.setResourceGroupName(requestDto.getName());
        updateResourceGroup.setDescription(requestDto.getDesc());
        updateResourceGroup.setVersion(rawResourceGroup.getVersion());

        // 5. 数据库操作
        super.updateById(updateResourceGroup);

        compareObjBuilder.after(super.getById(resourceGroupId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    private void checkResourceGroupCode(ResourceGroupRequestDto requestDto, ResourceGroup rawResourceGroup) {
        if (Objects.nonNull(rawResourceGroup) && StringUtils.equals(requestDto.getCode(), rawResourceGroup.getResourceGroupCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<ResourceGroup>lambdaQuery().eq(ResourceGroup::getResourceGroupCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.RESOURCE_GROUP_MSG_1000, requestDto.getCode());
        }
    }

    private void checkIsSystemResourceGroup(String resourceGroupCode) {
        String consoleClientId =  systemSettingService.getSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID, String.class);
        if (StringUtils.equals(consoleClientId, resourceGroupCode)) {
            throw new BizException(MessageConstants.RESOURCE_GROUP_MSG_1001);
        }
    }
}
