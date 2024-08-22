package cn.opensrcdevelop.auth.biz.mapper;

import cn.opensrcdevelop.auth.biz.entity.UserAttr;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface UserAttrMapper extends BaseMapper<UserAttr> {

    List<UserAttr> searchUserAttrs(@Param("userId") String userId);

    Integer getMaxDisplaySeq();
}
