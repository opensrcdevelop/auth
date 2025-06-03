package cn.opensrcdevelop.auth.biz.mapper.identity;

import cn.opensrcdevelop.auth.biz.entity.identity.IdentitySourceRegistration;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface IdentitySourceRegistrationMapper extends BaseMapper<IdentitySourceRegistration> {

    IdentitySourceRegistration getRegistrationByCode(String code);

    List<IdentitySourceRegistration> getEnabledRegistrations();

    IPage<IdentitySourceRegistration> searchRegistrations(@Param("page") IPage<IdentitySourceRegistration> page, @Param("keyword") String keyword);
}
