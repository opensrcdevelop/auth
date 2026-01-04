package cn.opensrcdevelop.auth.biz.service.user.attr.dict.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.ChildDictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.dict.DictMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.dict.DictRepository;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
public class DictServiceImpl extends ServiceImpl<DictMapper, Dict> implements DictService {

    private final UserAttrService userAttrService;
    private final DictRepository dictRepository;

    @Resource
    private DictDataService dictDataService;

    /**
     * 创建字典
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.DICT,
            sysOperation = SysOperationType.CREATE,
            success = "创建了字典（{{ @linkGen.toLink(#dictId, T(ResourceType).DICT) }}）",
            fail = "创建字典（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createDict(DictRequestDto requestDto) {
        // 1. 检查字典标识是否存在
        checkDictCode(requestDto, null);

        // 2. 属性设置
        String dictId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("dictId", dictId);

        Dict dict = new Dict();
        dict.setDictId(dictId);
        dict.setDictName(requestDto.getName());
        dict.setDictCode(requestDto.getCode());
        dict.setDescription(requestDto.getDesc());

        // 3. 数据库操作
        super.save(dict);
    }

    /**
     * 更新字典
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.DICT,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了字典（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).DICT) }}）",
            fail = "修改字典（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).DICT) }}）失败"
    )
    @Transactional
    @Override
    public void updateDict(DictRequestDto requestDto) {
        String dictId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawDict = super.getById(dictId);
        if (Objects.isNull(rawDict)) {
            return;
        }
        compareObjBuilder.id(dictId);
        compareObjBuilder.before(rawDict);

        // 2. 检查字典标识是否存在
        checkDictCode(requestDto, rawDict);

        // 3. 属性设置
        Dict updateDict = new Dict();
        updateDict.setDictId(dictId);
        updateDict.setDictName(requestDto.getName());
        updateDict.setDictCode(requestDto.getCode());
        updateDict.setDescription(requestDto.getDesc());

        // 4. 数据库操作
        super.updateById(updateDict);

        compareObjBuilder.after(super.getById(dictId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 获取字典详情
     *
     * @param dictId 字典ID
     * @return 字典详情
     */
    @Override
    public DictResponseDto detail(String dictId) {
        DictResponseDto dictResponse = new DictResponseDto();
        // 1. 查询数据库
        Dict dict = super.getById(dictId);
        if (Objects.isNull(dict)) {
            return dictResponse;
        }

        // 2. 属性设置
        dictResponse.setId(dict.getDictId());
        dictResponse.setName(dict.getDictName());
        dictResponse.setCode(dict.getDictCode());
        dictResponse.setDesc(dict.getDescription());

        return dictResponse;
    }

    /**
     * 获取字典列表
     *
     * @param page          页数
     * @param size          条数
     * @param keyword       字典名称 / 标识检索关键字
     * @param queryChildren 是否查询子字典
     * @return 字典列表
     */
    @Override
    public PageData<DictResponseDto> list(int page, int size, String keyword, boolean queryChildren) {
        // 1. 查询数据库
        Page<Dict> pageRequest = new Page<>(page, size);
        List<Dict> dicts;
        if (StringUtils.isNotEmpty(keyword)) {
            dicts = super.list(pageRequest, Wrappers.<Dict>lambdaQuery().like(Dict::getDictName, keyword).or(o -> o.like(Dict::getDictCode, keyword)).orderByAsc(Dict::getDictCode));
        } else {
            dicts = super.list(pageRequest, Wrappers.<Dict>lambdaQuery().orderByAsc(Dict::getDictCode));
        }

        // 2. 查询关联的全部子字典
        Map<String, List<Dict>> dictChildrenMap = new HashMap<>();
        if (queryChildren) {
            for (Dict dict : dicts) {
                List<Dict> children = dictRepository.selectByParentId(dict.getDictId());
                dictChildrenMap.put(dict.getDictId(), children);
            }
        }

        // 3. 属性设置
        PageData<DictResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setPages(pageRequest.getPages());

        var records = CommonUtil.stream(dicts).map(dict -> {
            DictResponseDto dictResponse = new DictResponseDto();
            dictResponse.setId(dict.getDictId());
            dictResponse.setName(dict.getDictName());
            dictResponse.setCode(dict.getDictCode());
            dictResponse.setParentId(dict.getParentDictId());
            dictResponse.setUniqueKey(UUID.randomUUID().toString());

            if (queryChildren) {
                List<DictResponseDto> children = CommonUtil.stream(dictChildrenMap.get(dict.getDictId()))
                        .map(child -> {
                            DictResponseDto childResponse = new DictResponseDto();
                            childResponse.setId(child.getDictId());
                            childResponse.setName(child.getDictName());
                            childResponse.setCode(child.getDictCode());
                            childResponse.setParentId(child.getParentDictId());
                            childResponse.setUniqueKey(UUID.randomUUID().toString());
                            return childResponse;
                        }).toList();
                if (CollectionUtils.isNotEmpty(children)) {
                    // 3.1 构建子字典树
                    List<DictResponseDto> rootChildren = CommonUtil.makeTree(children,
                            DictResponseDto::getParentId,
                            DictResponseDto::getId,
                            d -> dict.getDictId().equals(d.getParentId()),
                            DictResponseDto::setChildren,
                            2,
                            DictResponseDto::setLevel);
                    dictResponse.setChildren(rootChildren);
                } else {
                    dictResponse.setChildren(null);
                }

                dictResponse.setLevel(1);
            }

            return dictResponse;
        }).toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 删除字典
     *
     * @param dictId 字典ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.DICT,
            sysOperation = SysOperationType.DELETE,
            success = "删除了字典（{{ @linkGen.toLink(#dictId, T(ResourceType).DICT) }}）",
            fail = "删除字典（{{ @linkGen.toLink(#dictId, T(ResourceType).DICT) }}）失败"
    )
    @Transactional
    @Override
    public void removeDict(String dictId) {
        // 1. 获取子字典
        List<Dict> childDictList = dictRepository.selectByParentId(dictId);
        // 1.1 删除子字典关联关系
        if (CollectionUtils.isNotEmpty(childDictList)) {
            super.lambdaUpdate().set(Dict::getParentDictId, null).in(Dict::getDictId, CommonUtil.stream(childDictList).map(Dict::getDictId).toList());
        }

        // 2. 删除字典
        super.removeById(dictId);

        // 3. 删除关联的字典数据
        List<DictData> dictData = dictDataService.list(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId));
        if (CollectionUtils.isNotEmpty(dictData)) {
            dictDataService.removeDictData(CommonUtil.stream(dictData).map(DictData::getDataId).toList());
        }

        // 4. 删除关联的用户属性
        UserAttr userAttr = userAttrService.getOne(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getAttrDataType, UserAttrDataTypeEnum.DICT.getType()).and(o -> o.eq(UserAttr::getDictId, dictId)));
        if (Objects.nonNull(userAttr)) {
            userAttrService.removeUserAttr(userAttr.getAttrId());
        }

        // 5. 删除启用的字典数据的缓存
        removeEnabledDictDataCacheWithParents(dictId);
    }

    /**
     * 查询可选择的子字典
     *
     * @param dictId 字典ID
     * @return 可选择的子字典列表
     */
    @Override
    public List<DictResponseDto> listSelectableChildren(String dictId) {
        // 1. 查询所有父字典
        List<Dict> parentDicts = dictRepository.getAllParentDicts(dictId);

        // 2. 查询所有无父字典的字典
        List<Dict> noParentDicts = super.list(Wrappers.<Dict>lambdaQuery().isNull(Dict::getParentDictId));

        // 3. 筛选出不在父字典列表中的字典
        return CommonUtil.stream(noParentDicts)
                .filter(dict -> !dictId.equals(dict.getDictId()))
                .filter(noParentDict -> CommonUtil.stream(parentDicts).noneMatch(parentDict -> parentDict.getDictId().equals(noParentDict.getDictId())))
                .map(dict -> {
                    DictResponseDto dictResponse = new DictResponseDto();
                    dictResponse.setId(dict.getDictId());
                    dictResponse.setName(dict.getDictName());
                    dictResponse.setCode(dict.getDictCode());

                    return dictResponse;
                })
                .toList();
    }

    /**
     * 添加子字典
     *
     * @param requestDtoList 请求列表
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.DICT,
            sysOperation = SysOperationType.UPDATE,
            success = "为字典（{{ @linkGen.toLink(#parentDictId, T(ResourceType).DICT) }}）添加了子字典：{{ @linkGen.toLinks(childDictIds, T(ResourceType).DICT) }}" +
                    ", 关联的字典数据：{{ @linkGen.toLinks(relatedDictDataIds, T(ResourceType).DICT_DATA) }}",
            fail = "为字典（{{ @linkGen.toLink(#parentDictId, T(ResourceType).DICT) }}）添加子字典失败"
    )
    @Override
    public void addChildDicts(List<ChildDictRequestDto> requestDtoList) {
        // 1. 获取父字典
        String parentDictId = requestDtoList.getFirst().getId();
        AuditContext.setSpelVariable("parentDictId", parentDictId);
        Dict parentDict = super.getById(parentDictId);
        if (Objects.isNull(parentDict)) {
            return;
        }

        // 2. 获取可选的子字典ID和可关联的字典数据ID
        List<String> selectableChildDictIds = CommonUtil.stream(listSelectableChildren(parentDictId)).map(DictResponseDto::getId).toList();
        List<String> relatableDictDataIds = CommonUtil.stream(dictDataService.getRelatableDictData(parentDictId)).map(DictDataResponseDto::getId).toList();

        // 3. 更新子字典的父字典ID和关联的字典数据ID
        List<Dict> updateDicts = new ArrayList<>();
        List<String> childDictIds = new ArrayList<>();
        List<String> relatedDictDataIds = new ArrayList<>();
        for (ChildDictRequestDto requestDto : requestDtoList) {
            if (!selectableChildDictIds.contains(requestDto.getChildId()) || !relatableDictDataIds.contains(requestDto.getDataId())) {
                continue;
            }

            Dict updateDict = new Dict();
            updateDict.setDictId(requestDto.getChildId());
            updateDict.setParentDictId(parentDictId);
            updateDict.setRelatedDictDataId(requestDto.getDataId());
            updateDicts.add(updateDict);

            childDictIds.add(requestDto.getChildId());
            relatedDictDataIds.add(requestDto.getDataId());
        }
        super.saveOrUpdateBatch(updateDicts);

        AuditContext.setSpelVariable("childDictIds", childDictIds);
        AuditContext.setSpelVariable("relatedDictDataIds", relatedDictDataIds);
    }

    /**
     * 删除子字典
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.DICT,
            sysOperation = SysOperationType.DELETE,
            success = "移除了字典（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).DICT) }}）的子字典：{{ @linkGen.toLinks(requestDto.children, T(ResourceType).DICT) }}",
            fail = "移除字典（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).DICT) }}）的子字典失败"
    )
    @CacheEvict(
            cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA,
            key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0].id)"
    )
    @Override
    public void removeChildDict(ChildDictRequestDto requestDto) {
        // 1. 获取父字典
        Dict parentDict = super.getById(requestDto.getId());
        if (Objects.isNull(parentDict)) {
            return;
        }

        // 2. 获取子字典的子字典
        List<String> childDictIds = new ArrayList<>(CommonUtil.stream(dictRepository.selectByParentId(requestDto.getChildId())).map(Dict::getDictId).toList());
        childDictIds.add(requestDto.getChildId());

        // 3. 删除子字典的父字典ID和关联的字典数据ID
        List<Dict> updateDicts = CommonUtil.stream(childDictIds).distinct().map(childId -> {
            Dict updateDict = new Dict();
            updateDict.setDictId(childId);
            updateDict.setParentDictId(null);
            updateDict.setRelatedDictDataId(null);
            return updateDict;
        }).toList();
        super.saveOrUpdateBatch(updateDicts);
    }

    /**
     * 判断字典是否有子字典
     *
     * @param dictId 字典ID
     * @return 是否有子字典
     */
    @Override
    public boolean hasChildDict(String dictId) {
        return super.exists(Wrappers.<Dict>lambdaQuery().eq(Dict::getParentDictId, dictId));
    }

    public String generateEnabledDictDataCacheKey(String dictId) {
        return TenantContextHolder.getTenantContext().getTenantCode() + ":" + dictId;
    }

    private void removeEnabledDictDataCacheWithParents(String dictId) {
        // 1. 获取所有字典ID
        List<String> dictIds = new ArrayList<>(CommonUtil.stream(dictRepository.getAllParentDicts(dictId)).map(Dict::getDictId).toList());
        dictIds.add(dictId);

        // 2. 拼接缓存键
        List<String> cacheKeyList = new ArrayList<>();
        for (String id : dictIds) {
            cacheKeyList.add(CacheConstants.CACHE_ENABLED_DICT_DATA + "::" + generateEnabledDictDataCacheKey(id));
        }

        // 3. 删除缓存
        RedisUtil.delete(cacheKeyList.toArray(new String[0]));
    }

    private void checkDictCode(DictRequestDto requestDto, Dict rawDict) {
        if (Objects.nonNull(rawDict) && rawDict.getDictCode().equals(requestDto.getCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<Dict>lambdaQuery().eq(Dict::getDictCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.DICT_MSG_1000, requestDto.getCode());
        }
    }
}
