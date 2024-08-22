package cn.opensrcdevelop.auth.biz.mapper;

import cn.opensrcdevelop.auth.biz.entity.OidcScope;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface OidcScopeMapper extends BaseMapper<OidcScope> {

    List<OidcScope> searchScopeClaims(@Param("scopes") List<String> scopes);
}
