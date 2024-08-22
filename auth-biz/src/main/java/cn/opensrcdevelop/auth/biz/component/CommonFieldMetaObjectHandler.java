package cn.opensrcdevelop.auth.biz.component;

import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

/**
 * MP 数据库公共字段处理
 */
@Component
public class CommonFieldMetaObjectHandler implements MetaObjectHandler {

    @Override
    public void insertFill(MetaObject metaObject) {
        this.strictInsertFill(metaObject, CommonConstants.CREATE_TIME, LocalDateTime.class, LocalDateTime.now());
        this.strictInsertFill(metaObject, CommonConstants.VERSION, Integer.class, 1);
        AuthUtil.getCurrentUsername().ifPresent(username -> this.strictInsertFill(metaObject, CommonConstants.CREATE_BY, String.class, username));
    }

    @Override
    public void updateFill(MetaObject metaObject) {
        this.strictUpdateFill(metaObject, CommonConstants.UPDATE_TIME, LocalDateTime.class, LocalDateTime.now());
        AuthUtil.getCurrentUsername().ifPresent(username -> this.strictUpdateFill(metaObject, CommonConstants.UPDATE_BY, String.class, username));
    }
}
