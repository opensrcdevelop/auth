package cn.opensrcdevelop.auth.biz.repository.user.attr.dict;

import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;

import java.util.List;

public interface DictRepository {

    List<Dict> selectByParentId(String parentId);

    List<Dict> getAllParentDicts(String dictId);
}
