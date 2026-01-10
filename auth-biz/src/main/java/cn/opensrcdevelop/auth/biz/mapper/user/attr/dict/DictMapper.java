package cn.opensrcdevelop.auth.biz.mapper.user.attr.dict;

import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface DictMapper extends BaseMapper<Dict> {

    List<Dict> selectByParentId(@Param("parentId") String parentId);

    List<Dict> getAllParentDicts(@Param("dictId") String dictId);
}
