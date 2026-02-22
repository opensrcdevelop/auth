package cn.opensrcdevelop.auth.biz.mapper.client.oidc;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaim;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface OidcClaimMapper extends BaseMapper<OidcClaim> {

    List<OidcClaim> searchClaims(@Param("claims") List<String> claims);
}
