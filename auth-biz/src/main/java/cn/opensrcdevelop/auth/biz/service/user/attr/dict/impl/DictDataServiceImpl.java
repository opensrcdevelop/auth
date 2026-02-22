package cn.opensrcdevelop.auth.biz.service.user.attr.dict.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.dict.DictDataMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.dict.DictDatRepository;
import cn.opensrcdevelop.auth.biz.repository.user.attr.dict.DictRepository;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrMappingService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictService;
import cn.opensrcdevelop.common.cache.annoation.CacheExpire;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class DictDataServiceImpl extends ServiceImpl<DictDataMapper, DictData> implements DictDataService {

    private final DictDatRepository dictDataRepository;
    private final UserAttrMappingService userAttrMappingService;
    private final DictRepository dictRepository;

    @Resource
    @Lazy
    private DictService dictService;

    /**
     * 创建字典数据
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.DICT_DATA, sysOperation = SysOperationType.CREATE, success = "向字典（{{ @linkGen.toLink(#requestDto.dictId, T(ResourceType).DICT) }}）"
            +
            "中添加了数据（{{ @linkGen.toLink(#dictDataId, T(ResourceType).DICT_DATA) }}）", fail = "向字典（{{ @linkGen.toLink(#requestDto.dictId, T(ResourceType).DICT) }}）"
                    +
                    "中添加数据（{{ @linkGen.toLink(#dictDataId, T(ResourceType).DICT_DATA) }}）失败")
    @CacheEvict(cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA, key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0].dictId)")
    @Transactional
    @Override
    public void createDictData(DictDataRequestDto requestDto) {
        // 1. 检查字典数据值是否存在
        checkDictDataValue(requestDto, null);

        // 2. 属性设置
        String dictDataId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("dictDataId", dictDataId);

        DictData dictData = new DictData();
        dictData.setDictId(requestDto.getDictId());
        dictData.setDataId(dictDataId);
        dictData.setDataLabel(requestDto.getLabel());
        dictData.setDataValue(requestDto.getValue());
        CommonUtil.callSetWithCheck(Objects::nonNull, dictData::setEnable, requestDto::getEnable);
        setDictDataDisplaySeq(dictData, requestDto.getDisplaySeq(), requestDto.getDictId());

        // 3. 数据库操作
        super.save(dictData);
    }

    /**
     * 更新字典数据
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.DICT_DATA, sysOperation = SysOperationType.UPDATE, success = "修改了字典（{{ @linkGen.toLink(#requestDto.dictId, T(ResourceType).DICT) }}）"
            +
            "中的数据（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).DICT_DATA) }}）", fail = "修改字典（{{ @linkGen.toLink(#requestDto.dictId, T(ResourceType).DICT) }}）"
                    +
                    "中的数据（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).DICT_DATA) }}）失败")
    @Transactional
    @Override
    public void updateDictData(DictDataRequestDto requestDto) {
        String dictDataId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取原字典数据
        var rawDictData = super.getById(dictDataId);
        if (Objects.isNull(rawDictData)) {
            return;
        }
        compareObjBuilder.id(dictDataId);
        compareObjBuilder.before(rawDictData);

        // 2. 检查字典数据值是否存在
        checkDictDataValue(requestDto, rawDictData);

        // 3. 属性设置
        DictData updateDictData = new DictData();
        updateDictData.setDataId(requestDto.getId());
        updateDictData.setDataLabel(requestDto.getLabel());
        updateDictData.setDataValue(requestDto.getValue());
        CommonUtil.callSetWithCheck(Objects::nonNull, updateDictData::setEnable, requestDto::getEnable);
        setDictDataDisplaySeq(updateDictData, requestDto.getDisplaySeq(), requestDto.getDictId());
        // 3.1 禁用字典数据的场合下删除关联的用户属性值
        if (Boolean.FALSE.equals(requestDto.getEnable())) {
            userAttrMappingService.remove(
                    Wrappers.<UserAttrMapping>lambdaQuery().eq(UserAttrMapping::getAttrValue, requestDto.getId()));
        }

        // 4. 数据库操作
        super.updateById(updateDictData);

        // 5. 删除缓存
        removeEnabledDictDataCacheWithParents(requestDto.getDictId());

        compareObjBuilder.after(super.getById(dictDataId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 获取字典数据详情
     *
     * @param dictDataId
     *            字典数据ID
     * @return 字典数据详情
     */
    @Override
    public DictDataResponseDto detail(String dictDataId) {
        DictDataResponseDto dictDataResponse = new DictDataResponseDto();
        // 1. 查询数据库
        DictData dictData = super.getById(dictDataId);
        if (Objects.isNull(dictData)) {
            return dictDataResponse;
        }

        // 2. 属性设置
        dictDataResponse.setDictId(dictData.getDictId());
        dictDataResponse.setId(dictData.getDataId());
        dictDataResponse.setLabel(dictData.getDataLabel());
        dictDataResponse.setValue(dictData.getDataValue());
        dictDataResponse.setEnable(dictData.getEnable());
        dictDataResponse.setDisplaySeq(dictData.getDisplaySeq());
        return dictDataResponse;
    }

    /**
     * 获取字典数据列表
     *
     * @param dictId
     *            字典ID
     * @param page
     *            页数
     * @param size
     *            条数
     * @param keyword
     *            字典数据标签 / 值检索关键字
     * @return 字典数据列表
     */
    @Override
    public PageData<DictDataResponseDto> list(String dictId, int page, int size, String keyword) {
        // 1. 查询数据库
        Page<DictData> pageRequest = new Page<>(page, size);
        List<DictData> dictDatas;
        if (StringUtils.isNotEmpty(keyword)) {
            dictDatas = super.list(pageRequest, Wrappers.<DictData>lambdaQuery()
                    .eq(DictData::getDictId, dictId)
                    .and(o -> o.like(DictData::getDataLabel, keyword).or().like(DictData::getDataValue, keyword))
                    .orderByAsc(DictData::getDisplaySeq));
        } else {
            dictDatas = super.list(pageRequest, Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId)
                    .orderByAsc(DictData::getDisplaySeq));
        }

        // 2. 属性设置
        PageData<DictDataResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setSize(pageRequest.getSize());
        pageData.setCurrent(pageRequest.getCurrent());

        var records = CommonUtil.stream(dictDatas).map(this::convertDictData2RepDto).toList();
        pageData.setList(records);

        return pageData;
    }

    /**
     * 删除字典数据
     *
     * @param dictDataIds
     *            字典数据ID集合
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.DICT_DATA, sysOperation = SysOperationType.DELETE, success = "删除了字典数据（{{ @linkGen.toLinks(#dictDataIds, T(ResourceType).DICT_DATA) }}）", fail = "删除字典数据（{{ @linkGen.toLinks(#dictDataIds, T(ResourceType).DICT_DATA) }}）失败")
    @CacheEvict(cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA, key = "#root.target.generateEnabledDictDataCacheKeyById(#root.args[0])", condition = "#root.args[0]?.size() > 0", beforeInvocation = true)
    @Transactional
    @Override
    public void removeDictData(List<String> dictDataIds) {
        // 1. 删除字典数据
        super.removeBatchByIds(dictDataIds);

        // 2. 删除关联的用户属性映射
        userAttrMappingService
                .remove(Wrappers.<UserAttrMapping>lambdaQuery().in(UserAttrMapping::getAttrValue, dictDataIds));

        // 3. 删除与子字典的关联关系
        dictService.lambdaUpdate().set(Dict::getRelatedDictDataId, null).set(Dict::getParentDictId, null)
                .in(Dict::getRelatedDictDataId, dictDataIds);
    }

    /**
     * 获取启用的字典数据
     *
     * @param dictId
     *            字典ID
     * @return 启用的字典数据
     */
    @Cacheable(cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA, key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0])")
    @CacheExpire("7 * 24 * 3600")
    @Override
    public List<DictDataResponseDto> getEnabledDictData(String dictId) {
        // 1. 获取字典及其子字典
        List<Dict> allDicts = new ArrayList<>(dictRepository.selectByParentId(dictId));
        allDicts.add(dictService.getById(dictId));

        // 2. 获取所有启用的字典数据
        List<DictData> allDictData = super.list(Wrappers.<DictData>lambdaQuery().in(DictData::getDictId,
                CommonUtil.stream(allDicts).map(Dict::getDictId).toList()).eq(DictData::getEnable, true)
                .orderByAsc(DictData::getDisplaySeq));

        // 3. 移除关联的父字典数据没有启用的字典数据
        Map<String, Dict> dictMap = allDicts.stream().collect(Collectors.toMap(Dict::getDictId, d -> d));
        Map<String, DictData> dictDataMap = allDictData.stream().collect(Collectors.toMap(DictData::getDataId, d -> d));
        List<DictData> filteredDictData = CommonUtil.stream(allDictData).filter(x -> {
            // 3.1 获取对应的字典
            Dict dict = dictMap.get(x.getDictId());
            // 3.2 检查是否有关联的父字典数据
            return dict.getParentDictId() == null || dictDataMap.containsKey(dict.getRelatedDictDataId());
        }).toList();

        // 4. 属性编辑
        List<DictDataResponseDto> allDictDataResponseDto = CommonUtil.stream(filteredDictData).map(dictData -> {
            DictDataResponseDto responseDto = new DictDataResponseDto();
            responseDto.setId(dictData.getDataId());
            responseDto.setValue(dictData.getDataValue());
            responseDto.setLabel(dictData.getDataLabel());
            responseDto.setDisplaySeq(dictData.getDisplaySeq());
            responseDto.setParentId(CommonUtil.stream(allDicts).filter(x -> x.getDictId().equals(dictData.getDictId()))
                    .findFirst().orElse(new Dict()).getRelatedDictDataId());

            return responseDto;
        }).toList();

        return CommonUtil.makeTree(
                allDictDataResponseDto,
                DictDataResponseDto::getParentId,
                DictDataResponseDto::getId,
                x -> Objects.isNull(x.getParentId()),
                DictDataResponseDto::setChildren,
                null,
                null);
    }

    /**
     * 获取可关联的字典数据
     *
     * @param dictId
     *            字典ID
     * @return 可关联的字典数据
     */
    @Override
    public List<DictDataResponseDto> getRelatableDictData(String dictId) {
        // 1. 获取全部字典数据
        List<DictData> allDictData = super.list(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId));

        if (CollectionUtils.isEmpty(allDictData)) {
            return Collections.emptyList();
        }

        // 2. 获取已关联的字典数据ID
        List<String> relatedDictIds = CommonUtil.stream(dictService.list(Wrappers.<Dict>lambdaQuery()
                .select(Dict::getRelatedDictDataId)
                .eq(Dict::getParentDictId, dictId))).map(Dict::getRelatedDictDataId).toList();

        // 3. 过滤已关联的字典数据
        List<DictData> relatableDictData = CommonUtil.stream(allDictData)
                .filter(x -> !relatedDictIds.contains(x.getDataId())).toList();

        return CommonUtil.stream(relatableDictData).map(this::convertDictData2RepDto).toList();
    }

    /**
     * 设置字典数据显示顺序
     *
     * @param dictData
     *            字典数据
     * @param expectedDisplaySeq
     *            期待的显示书顺序
     * @param dictId
     *            字典ID
     */
    private void setDictDataDisplaySeq(DictData dictData, Integer expectedDisplaySeq, String dictId) {
        if (Objects.isNull(expectedDisplaySeq)) {
            dictData.setDisplaySeq(dictDataRepository.getNextDisplaySeq(dictId));
        } else {
            // 将大于等于期待的显示顺序加 1
            super.update(Wrappers.<DictData>lambdaUpdate()
                    .setSql("display_seq = display_seq + 1")
                    .eq(DictData::getDictId, dictId)
                    .and(o -> o.ge(DictData::getDisplaySeq, expectedDisplaySeq)));
            dictData.setDisplaySeq(expectedDisplaySeq);
        }
    }

    private DictDataResponseDto convertDictData2RepDto(DictData dictData) {
        DictDataResponseDto dictDataResponse = new DictDataResponseDto();
        dictDataResponse.setId(dictData.getDataId());
        dictDataResponse.setLabel(dictData.getDataLabel());
        dictDataResponse.setValue(dictData.getDataValue());
        dictDataResponse.setEnable(dictData.getEnable());
        dictDataResponse.setDisplaySeq(dictData.getDisplaySeq());
        return dictDataResponse;
    }

    public String generateEnabledDictDataCacheKey(String dictId) {
        return TenantContextHolder.getTenantContext().getTenantCode() + ":" + dictId;
    }

    public String generateEnabledDictDataCacheKeyById(List<String> dictDataIds) {
        DictData dictData = super.getById(dictDataIds.getFirst());
        return Objects.nonNull(dictData)
                ? TenantContextHolder.getTenantContext().getTenantCode() + ":" + dictData.getDictId()
                : "";
    }

    private void removeEnabledDictDataCacheWithParents(String dictId) {
        // 1. 获取所有字典ID
        List<String> dictIds = new ArrayList<>(
                CommonUtil.stream(dictRepository.getAllParentDicts(dictId)).map(Dict::getDictId).toList());
        dictIds.add(dictId);

        // 2. 拼接缓存键
        List<String> cacheKeyList = new ArrayList<>();
        for (String id : dictIds) {
            cacheKeyList.add(CacheConstants.CACHE_ENABLED_DICT_DATA + "::" + generateEnabledDictDataCacheKey(id));
        }

        // 3. 删除缓存
        RedisUtil.delete(cacheKeyList.toArray(new String[0]));
    }

    private void checkDictDataValue(DictDataRequestDto requestDto, DictData rawDictData) {
        if (Objects.nonNull(rawDictData) && requestDto.getValue().equals(rawDictData.getDataValue())) {
            return;
        }

        if (Objects.isNull(dictService.getById(requestDto.getDictId()))) {
            throw new BizException(MessageConstants.DICT_DATA_MSG_1001);
        }

        if (Objects.nonNull(super.getOne(Wrappers.<DictData>lambdaQuery()
                .eq(DictData::getDictId, requestDto.getDictId()).eq(DictData::getDataValue, requestDto.getValue())))) {
            throw new BizException(MessageConstants.DICT_DATA_MSG_1000, requestDto.getValue());
        }
    }
}
