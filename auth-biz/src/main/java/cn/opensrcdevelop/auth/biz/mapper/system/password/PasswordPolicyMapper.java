package cn.opensrcdevelop.auth.biz.mapper.system.password;

import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicy;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface PasswordPolicyMapper extends BaseMapper<PasswordPolicy> {

    PasswordPolicy getMatchedPasswordPolicy(@Param("userId") String userId);
}
