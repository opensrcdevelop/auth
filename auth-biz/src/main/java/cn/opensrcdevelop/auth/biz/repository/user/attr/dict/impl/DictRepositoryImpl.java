package cn.opensrcdevelop.auth.biz.repository.user.attr.dict.impl;

import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.dict.DictMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.dict.DictRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class DictRepositoryImpl implements DictRepository {

    private final DictMapper dictMapper;

    /**
     * 根据父字典ID查询子字典列表
     *
     * @param parentId 父字典ID
     * @return 子字典列表
     */
    @Override
    public List<Dict> selectByParentId(String parentId) {
        return dictMapper.selectByParentId(parentId);
    }

    /**
     * 根据字典ID查询所有父字典列表
     *
     * @param dictId 字典ID
     * @return 所有父字典列表
     */
    public List<Dict> getAllParentDicts(String dictId) {
        return dictMapper.getAllParentDicts(dictId);
    }
}
