package cn.opensrcdevelop.auth.biz.mapper.user;

import cn.opensrcdevelop.auth.biz.entity.user.LoginLog;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface LoginLogMapper extends BaseMapper<LoginLog> {
}
