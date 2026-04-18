package cn.opensrcdevelop.auth.biz.mapper.permission;

import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequest;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface PermissionRequestMapper extends BaseMapper<PermissionRequest> {

    IPage<PermissionRequest> searchPermissionRequests(@Param("page") IPage<PermissionRequest> page,
            @Param("userIds") List<String> userIds, @Param("usernameSearchKeyword") String usernameSearchKeyword);
}
