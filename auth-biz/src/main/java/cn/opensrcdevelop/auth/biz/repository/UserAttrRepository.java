package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.UserAttr;

import java.util.List;

public interface UserAttrRepository {

    List<UserAttr> searchUserAttrs(String userId);

    Integer getMaxDisplaySeq();
}
