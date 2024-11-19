package cn.opensrcdevelop.auth.biz.service.impl;

import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.DictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.DictResponseDto;
import cn.opensrcdevelop.auth.biz.entity.Dict;
import cn.opensrcdevelop.auth.biz.entity.DictData;
import cn.opensrcdevelop.auth.biz.entity.UserAttr;
import cn.opensrcdevelop.auth.biz.mapper.DictMapper;
import cn.opensrcdevelop.auth.biz.service.DictDataService;
import cn.opensrcdevelop.auth.biz.service.DictService;
import cn.opensrcdevelop.auth.biz.service.UserAttrService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
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
public class DictServiceImpl extends ServiceImpl<DictMapper, Dict> implements DictService {

    private final DictDataService dictDataService;
    private final UserAttrService userAttrService;

    /**
     * 创建字典
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createDict(DictRequestDto requestDto) {
        // 1. 属性设置
        Dict dict = new Dict();
        dict.setDictId(CommonUtil.getUUIDString());
        dict.setDictName(requestDto.getName());
        dict.setDictCode(requestDto.getCode());
        dict.setDescription(requestDto.getDesc());

        // 2. 数据库操作
        super.save(dict);
    }

    /**
     * 更新字典
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updateDict(DictRequestDto requestDto) {
        // 1. 获取版本号
        var rawDict = super.getById(requestDto.getId());
        if (Objects.isNull(rawDict)) {
            return;
        }

        // 2. 属性设置
        Dict updateDict = new Dict();
        updateDict.setDictId(requestDto.getId());
        updateDict.setDictName(requestDto.getName());
        updateDict.setDictCode(requestDto.getCode());
        updateDict.setDescription(requestDto.getDesc());

        // 3. 数据库操作
        super.updateById(updateDict);
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
    @Transactional
    @Override
    public void removeDict(String dictId) {
        // 1. 删除字典
        super.removeById(dictId);

        // 2. 删除关联的字典数据
        List<DictData> dictDatas = dictDataService.list(Wrappers.<DictData>lambdaQuery().eq(DictData::getDictId, dictId));
        if (CollectionUtils.isNotEmpty(dictDatas)) {
            dictDataService.removeDictData(CommonUtil.stream(dictDatas).map(DictData::getDataId).toList());
        }

        // 3. 删除关联的用户属性
        UserAttr userAttr = userAttrService.getOne(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getAttrDataType, UserAttrDataTypeEnum.DICT.getType()).and(o -> o.eq(UserAttr::getDictId, dictId)));
        if (Objects.nonNull(userAttr)) {
            userAttrService.removeUserAttr(userAttr.getAttrId());
        }
    }
}
