package cn.opensrcdevelop.auth.biz.service.audit.impl;

import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.audit.entity.ObjChangeLog;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.audit.ObjChangeLogResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mapper.audit.ObjChangeLogMapper;
import cn.opensrcdevelop.auth.biz.service.audit.AuditLogService;
import cn.opensrcdevelop.auth.biz.service.audit.ObjChangeLogService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class ObjChangeLogServiceImpl extends ServiceImpl<ObjChangeLogMapper, ObjChangeLog> implements ObjChangeLogService {

    private final AuditLogService auditLogService;
    private final UserAttrService userAttrService;

    /**
     * 获取对象变更日志
     *
     * @param auditId 审计ID
     * @return 对象变更日志
     */
    @Override
    public ObjChangeLogResponseDto getObjChangeLog(String auditId) {
        ObjChangeLogResponseDto changeLogResponseDto = new ObjChangeLogResponseDto();

        // 1. 数据库操作
        AuditLog auditLog = auditLogService.getById(auditId);
        if (Objects.isNull(auditLog)) {
            throw new BizException(MessageConstants.AUDIT_LOG_MSG_1000);
        }
        List<ObjChangeLog> objChangeLogs = super.list(Wrappers.<ObjChangeLog>lambdaQuery().eq(ObjChangeLog::getAuditId, auditId));
        if (CollectionUtils.isEmpty(objChangeLogs)) {
            throw new BizException(MessageConstants.AUDIT_LOG_MSG_1001);
        }

        // 2. 获取变更内容
        ResourceType resourceType = ResourceType.getById(auditLog.getResourceId());
        List<String> changes = CommonUtil.stream(objChangeLogs)
                .map(objChangeLog -> toCompareObj(objChangeLog, resourceType))
                .map(CompareObj::getChangText).toList();
        changeLogResponseDto.setChanges(changes);

        return changeLogResponseDto;
    }

    private CompareObj<?> toCompareObj(ObjChangeLog objChangeLog, ResourceType resourceType) {
        try {
            var compareObjBuilder = CompareObj.builder();
            Class<?> clazz = Class.forName(objChangeLog.getJavaType());

            compareObjBuilder.id(objChangeLog.getObjId());
            if (Objects.nonNull(objChangeLog.getBefore())) {
                compareObjBuilder.before(CommonUtil.deserializeObject(objChangeLog.getBefore(), clazz));
            }

            if (Objects.nonNull(objChangeLog.getAfter())) {
                compareObjBuilder.after(CommonUtil.deserializeObject(objChangeLog.getAfter(), clazz));
            }

            if (ResourceType.USER.equals(resourceType)) {
                Map<String, String> propertyNames = new HashMap<>();
                CommonUtil.stream(userAttrService.list()).forEach(userAttr -> propertyNames.put(userAttr.getAttrKey(), userAttr.getAttrName()));
                compareObjBuilder.entityName(ResourceType.USER.getName());
                compareObjBuilder.propertyNames(propertyNames);
                compareObjBuilder.excludeProperty(List.of(CommonConstants.ROLES, CommonUtil.extractFileNameFromGetter(User::getCreateTime)));
            }

            return compareObjBuilder.build();
        } catch (ClassNotFoundException ex) {
            throw new ServerException("获取变更日志对象类型失败", ex);
        }
    }
}
