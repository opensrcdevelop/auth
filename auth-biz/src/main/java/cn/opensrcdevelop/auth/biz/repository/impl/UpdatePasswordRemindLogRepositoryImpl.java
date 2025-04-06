package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.UpdatePasswordRemindLog;
import cn.opensrcdevelop.auth.biz.mapper.UpdatePasswordRemindLogMapper;
import cn.opensrcdevelop.auth.biz.repository.UpdatePasswordRemindLogRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class UpdatePasswordRemindLogRepositoryImpl implements UpdatePasswordRemindLogRepository {

    private final UpdatePasswordRemindLogMapper updatePasswordRemindLogMapper;


    /**
     * 检索修改密码提醒日志
     *
     *
     * @param page 分页对象
     * @param keyword 搜索关键字
     */
    @Override
    public void searchRemindLogs(IPage<UpdatePasswordRemindLog> page, String keyword) {
        updatePasswordRemindLogMapper.searchRemindLogs(page, keyword);
    }
}
