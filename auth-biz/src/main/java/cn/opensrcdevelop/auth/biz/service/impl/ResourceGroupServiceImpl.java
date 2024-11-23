package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.ResourceGroupRequestDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceGroupResponseDto;
import cn.opensrcdevelop.auth.biz.dto.ResourceResponseDto;
import cn.opensrcdevelop.auth.biz.entity.Resource;
import cn.opensrcdevelop.auth.biz.entity.ResourceGroup;
import cn.opensrcdevelop.auth.biz.mapper.ResourceGroupMapper;
import cn.opensrcdevelop.auth.biz.service.ResourceGroupService;
import cn.opensrcdevelop.auth.biz.service.ResourceService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
public class ResourceGroupServiceImpl extends ServiceImpl<ResourceGroupMapper, ResourceGroup> implements ResourceGroupService {

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
     * @param resourceGroupIds 资源组ID集合
     */
    @Transactional
    @Override
    public void removeResourceGroup(List<String> resourceGroupIds) {
        // 1. 删除资源组
        super.remove(Wrappers.<ResourceGroup>lambdaQuery().in(ResourceGroup::getResourceGroupId, resourceGroupIds));

        // 2. 删除组内资源
        var resources = resourceService.list(Wrappers.<Resource>lambdaQuery().in(Resource::getResourceGroupId, resourceGroupIds));
        if (CollectionUtils.isNotEmpty(resources)) {
            resourceService.removeResource(resources.stream().map(Resource::getResourceId).toList());
        }
    }

    /**
     * 创建资源组
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createResourceGroup(ResourceGroupRequestDto requestDto) {
        // 1. 检查资源组标识是否存在
        checkResourceGroupCode(requestDto, null);

        // 2. 属性编辑
        ResourceGroup resourceGroup = new ResourceGroup();
        resourceGroup.setResourceGroupId(CommonUtil.getUUIDString());
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
    @Transactional
    @Override
    public void updateResourceGroup(ResourceGroupRequestDto requestDto) {
        // 1. 获取版本号
        var rawResourceGroup = super.getById(requestDto.getId());
        if (Objects.isNull(rawResourceGroup)) {
            return;
        }

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
    }

    private void checkResourceGroupCode(ResourceGroupRequestDto requestDto, ResourceGroup rawResourceGroup) {
        if (Objects.nonNull(rawResourceGroup) && StringUtils.equals(requestDto.getCode(), rawResourceGroup.getResourceGroupCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<ResourceGroup>lambdaQuery().eq(ResourceGroup::getResourceGroupCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.RESOURCE_GROUP_MSG_1000, requestDto.getCode());
        }
    }
}
