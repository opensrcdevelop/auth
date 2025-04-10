package cn.opensrcdevelop.auth.biz.service.user.attr.dict.impl;

import cn.opensrcdevelop.auth.biz.constants.CacheConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.dict.DictDataMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.dict.impl.DictDataRepositoryImpl;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrMappingService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictService;
import cn.opensrcdevelop.common.cache.annoation.CacheExpire;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class DictDataServiceImpl extends ServiceImpl<DictDataMapper, DictData> implements DictDataService {

    private final DictDataRepositoryImpl dictDataRepository;
    private final UserAttrMappingService userAttrMappingService;

    @Resource
    @Lazy
    private DictService dictService;

    /**
     * 创建字典数据
     *
     * @param requestDto 请求
     */
    @CacheEvict(
            cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA,
            key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0].dictId)"
    )
    @Transactional
    @Override
    public void createDictData(DictDataRequestDto requestDto) {
        // 1. 检查字典数据值是否存在
        checkDictDataValue(requestDto, null);

        // 2. 属性设置
        DictData dictData = new DictData();
        dictData.setDictId(requestDto.getDictId());
        dictData.setDataId(CommonUtil.getUUIDString());
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
     * @param requestDto 请求
     */
    @CacheEvict(
            cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA,
            key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0].dictId)"
    )
    @Transactional
    @Override
    public void updateDictData(DictDataRequestDto requestDto) {
        // 1. 获取原字典数据
        var rawDictData = super.getById(requestDto.getId());
        if (Objects.isNull(rawDictData)) {
            return;
        }

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
            userAttrMappingService.remove(Wrappers.<UserAttrMapping>lambdaQuery().eq(UserAttrMapping::getAttrValue, requestDto.getId()));
        }

        // 4. 数据库操作
        super.updateById(updateDictData);
    }

    /**
     * 获取字典数据详情
     *
     * @param dictDataId 字典数据ID
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
     * @param dictId 字典ID
     * @param page 页数
     * @param size 条数
     * @param keyword 字典数据标签 / 值检索关键字
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
            dictDatas = super.list(pageRequest, Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId).orderByAsc(DictData::getDisplaySeq));
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
     * @param dictDataIds 字典数据ID集合
     */
    @CacheEvict(
            cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA,
            key = "#root.target.generateEnabledDictDataCacheKeyById(#root.args[0])",
            condition = "#root.args[0]?.size() > 0",
            beforeInvocation = true
    )
    @Transactional
    @Override
    public void removeDictData(List<String> dictDataIds) {
        // 1. 删除字典数据
        super.removeBatchByIds(dictDataIds);

        // 2. 删除关联的用户属性映射
        userAttrMappingService.remove(Wrappers.<UserAttrMapping>lambdaQuery().in(UserAttrMapping::getAttrValue, dictDataIds));
    }

    /**
     * 获取启用的字典数据
     *
     * @param dictId 字典ID
     * @return 启用的字典数据
     */
    @Cacheable(
            cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA,
            key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0])"
    )
    @CacheExpire("7 * 24 * 3600")
    @Override
    public List<DictDataResponseDto> getEnabledDictData(String dictId) {
        // 1. 查询数据库
        var dictData = super.list(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId).eq(DictData::getEnable, true).orderByAsc(DictData::getDisplaySeq));

        // 2. 属性编辑
        return CommonUtil.stream(dictData).map(this::convertDictData2RepDto).toList();
    }

    /**
     * 设置字典数据显示顺序
     *
     * @param dictData 字典数据
     * @param expectedDisplaySeq 期待的显示书顺序
     * @param dictId 字典ID
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
        DictData dictData = super.getById(dictDataIds.get(0));
        return Objects.nonNull(dictData) ? TenantContextHolder.getTenantContext().getTenantCode() + ":" + dictData.getDictId() : "";
    }

    private void checkDictDataValue(DictDataRequestDto requestDto, DictData rawDictData) {
        if (Objects.nonNull(rawDictData) && StringUtils.equals(requestDto.getValue(), rawDictData.getDataValue())) {
            return;
        }

        if (Objects.isNull(dictService.getById(requestDto.getDictId()))) {
            throw new BizException(MessageConstants.DICT_DATA_MSG_1001);
        }

        if (Objects.nonNull(super.getOne(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, requestDto.getDictId()).eq(DictData::getDataValue, requestDto.getValue())))) {
            throw new BizException(MessageConstants.DICT_DATA_MSG_1000, requestDto.getValue());
        }
    }
}
