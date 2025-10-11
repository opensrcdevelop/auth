package cn.opensrcdevelop.auth.biz.mapper.audit;

import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDateTime;

@Mapper
public interface AuditLogMapper extends BaseMapper<AuditLog> {

    IPage<AuditLog> searchUserOperationLog(@Param("page") IPage<AuditLog> page,
                                           @Param("keyword") String keyword,
                                           @Param("operationType") Integer operationType,
                                           @Param("startTime")LocalDateTime startTime,
                                           @Param("endTime") LocalDateTime endTime);

    IPage<AuditLog> searchSysOperationLog(@Param("page") IPage<AuditLog> page,
                                          @Param("keyword") String keyword,
                                          @Param("operationType") Integer operationType,
                                          @Param("resourceId") String resourceId,
                                          @Param("startTime") LocalDateTime startTime,
                                          @Param("endTime") LocalDateTime endTime);
}
