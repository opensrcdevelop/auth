package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.UpdatePasswordRemindLog;
import com.baomidou.mybatisplus.core.metadata.IPage;

public interface UpdatePasswordRemindLogRepository {

    void searchRemindLogs(IPage<UpdatePasswordRemindLog> page, String keyword);
}
