package cn.opensrcdevelop.auth.biz.mapper.identity;

import cn.opensrcdevelop.auth.biz.entity.identity.ThirdAccount;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface ThirdAccountMapper extends BaseMapper<ThirdAccount> {

    IPage<ThirdAccount> searchUserBindings(@Param("page") IPage<ThirdAccount> page,
            @Param("registrationId") String registrationId, @Param("keyword") String keyword);
}
