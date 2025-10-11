package cn.opensrcdevelop.auth.biz.service.audit;

import cn.opensrcdevelop.auth.audit.entity.ObjChangeLog;
import cn.opensrcdevelop.auth.biz.dto.audit.ObjChangeLogResponseDto;
import com.baomidou.mybatisplus.extension.service.IService;

public interface ObjChangeLogService extends IService<ObjChangeLog> {

    ObjChangeLogResponseDto getObjChangeLog(String auditId);
}
