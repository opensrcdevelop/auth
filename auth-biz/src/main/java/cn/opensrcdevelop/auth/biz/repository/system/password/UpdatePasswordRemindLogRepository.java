package cn.opensrcdevelop.auth.biz.repository.system.password;

import cn.opensrcdevelop.auth.biz.entity.system.password.UpdatePasswordRemindLog;
import com.baomidou.mybatisplus.core.metadata.IPage;

public interface UpdatePasswordRemindLogRepository {

    void searchRemindLogs(IPage<UpdatePasswordRemindLog> page, String keyword);
}
