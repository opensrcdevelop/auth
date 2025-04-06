package cn.opensrcdevelop.auth.biz.mapper;

import cn.opensrcdevelop.auth.biz.entity.UpdatePasswordRemindLog;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface UpdatePasswordRemindLogMapper extends BaseMapper<UpdatePasswordRemindLog> {

    IPage<UpdatePasswordRemindLog> searchRemindLogs(@Param("page") IPage<UpdatePasswordRemindLog> page, @Param("keyword") String keyword);
}
