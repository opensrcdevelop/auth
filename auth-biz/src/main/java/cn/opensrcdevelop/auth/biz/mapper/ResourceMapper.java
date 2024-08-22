package cn.opensrcdevelop.auth.biz.mapper;

import cn.opensrcdevelop.auth.biz.entity.Resource;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface ResourceMapper extends BaseMapper<Resource> {

    IPage<Resource> searchResources(@Param("page") IPage<Resource> page, @Param("keyword") String keyword);
}
