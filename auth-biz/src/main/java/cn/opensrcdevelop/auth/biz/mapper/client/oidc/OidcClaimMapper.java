package cn.opensrcdevelop.auth.biz.mapper.client.oidc;

import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcClaim;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface OidcClaimMapper extends BaseMapper<OidcClaim> {

    List<OidcClaim> searchClaims(@Param("claims") List<String> claims);
}
