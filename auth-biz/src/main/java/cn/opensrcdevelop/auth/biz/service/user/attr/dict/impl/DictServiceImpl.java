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
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.dict.DictMapper;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class DictServiceImpl extends ServiceImpl<DictMapper, Dict> implements DictService {

    private final UserAttrService userAttrService;

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
     * @param page    页数
     * @param size    条数
     * @param keyword 字典名称 / 标识检索关键字
     * @return 字典列表
     */
    @Override
    public PageData<DictResponseDto> list(int page, int size, String keyword) {
        // 1. 查询数据库
        Page<Dict> pageRequest = new Page<>(page, size);
        List<Dict> dicts;
        if (StringUtils.isNotEmpty(keyword)) {
            dicts = super.list(pageRequest, Wrappers.<Dict>lambdaQuery().like(Dict::getDictName, keyword).or(o -> o.like(Dict::getDictCode, keyword)).orderByAsc(Dict::getDictCode));
        } else {
            dicts = super.list(pageRequest, Wrappers.<Dict>lambdaQuery().orderByAsc(Dict::getDictCode));
        }

        // 2. 属性设置
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
            dictResponse.setDataCnt(dictDataService.count(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dict.getDictId())));

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
    @CacheEvict(
            cacheNames = CacheConstants.CACHE_ENABLED_DICT_DATA,
            key = "#root.target.generateEnabledDictDataCacheKey(#root.args[0])"
    )
    @Transactional
    @Override
    public void removeDict(String dictId) {
        // 1. 删除字典
        super.removeById(dictId);

        // 2. 删除关联的字典数据
        List<DictData> dictData = dictDataService.list(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId));
        if (CollectionUtils.isNotEmpty(dictData)) {
            dictDataService.removeDictData(CommonUtil.stream(dictData).map(DictData::getDataId).toList());
        }

        // 3. 删除关联的用户属性
        UserAttr userAttr = userAttrService.getOne(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getAttrDataType, UserAttrDataTypeEnum.DICT.getType()).and(o -> o.eq(UserAttr::getDictId, dictId)));
        if (Objects.nonNull(userAttr)) {
            userAttrService.removeUserAttr(userAttr.getAttrId());
        }
    }

    public String generateEnabledDictDataCacheKey(String dictId) {
        return TenantContextHolder.getTenantContext().getTenantCode() + ":" + dictId;
    }

    private void  checkDictCode(DictRequestDto requestDto, Dict rawDict) {
        if (Objects.nonNull(rawDict) && StringUtils.equals(requestDto.getCode(), rawDict.getDictCode())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<Dict>lambdaQuery().eq(Dict::getDictCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.DICT_MSG_1000, requestDto.getCode());
        }
    }
}
