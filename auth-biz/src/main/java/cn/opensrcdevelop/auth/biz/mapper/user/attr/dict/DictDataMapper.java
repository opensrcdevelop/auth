package cn.opensrcdevelop.auth.biz.mapper.user.attr.dict;

import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface DictDataMapper extends BaseMapper<DictData> {

    Integer getNextDisplaySeq(String dictId);
}
