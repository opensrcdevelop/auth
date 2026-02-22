package cn.opensrcdevelop.auth.biz.repository.user.attr.dict.impl;

import cn.opensrcdevelop.auth.biz.mapper.user.attr.dict.DictDataMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.dict.DictDatRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class DictDataRepositoryImpl implements DictDatRepository {

    private final DictDataMapper mapper;

    /**
     * 获取下一个显示顺序
     *
     * @param dictId
     *            字典ID
     * @return 显示顺序
     */
    @Override
    public Integer getNextDisplaySeq(String dictId) {
        return mapper.getNextDisplaySeq(dictId);
    }
}
