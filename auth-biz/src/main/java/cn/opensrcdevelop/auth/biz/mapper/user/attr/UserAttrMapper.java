package cn.opensrcdevelop.auth.biz.mapper.user.attr;

import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface UserAttrMapper extends BaseMapper<UserAttr> {

    List<UserAttr> searchUserAttrs(@Param("userId") String userId);

    Integer getMaxDisplaySeq();
}
