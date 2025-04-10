package cn.opensrcdevelop.auth.biz.mapper.auth;

import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

@Mapper
public interface AuthorizeMapper extends BaseMapper<AuthorizeRecord> {
}
