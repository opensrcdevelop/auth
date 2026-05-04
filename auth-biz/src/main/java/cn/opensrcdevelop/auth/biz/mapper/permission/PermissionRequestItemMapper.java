package cn.opensrcdevelop.auth.biz.mapper.permission;

import cn.opensrcdevelop.auth.biz.entity.permission.PermissionRequestItem;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface PermissionRequestItemMapper extends BaseMapper<PermissionRequestItem> {

    List<PermissionRequestItem> getPermissionRequestItemsByRequestIdAndUserId(@Param("userId") String userId,
            @Param("requestId") String requestId);
}
