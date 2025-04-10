package cn.opensrcdevelop.auth.biz.repository.user.attr;

import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;

import java.util.List;

public interface UserAttrRepository {

    List<UserAttr> searchUserAttrs(String userId);

    Integer getMaxDisplaySeq();
}
