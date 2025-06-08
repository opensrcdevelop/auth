package cn.opensrcdevelop.auth.biz.service.system.password.impl;

import cn.opensrcdevelop.auth.biz.dto.system.password.UpdatePasswordRemindLogResponseDto;
import cn.opensrcdevelop.auth.biz.entity.system.password.UpdatePasswordRemindLog;
import cn.opensrcdevelop.auth.biz.mapper.system.password.UpdatePasswordRemindLogMapper;
import cn.opensrcdevelop.auth.biz.repository.system.password.UpdatePasswordRemindLogRepository;
import cn.opensrcdevelop.auth.biz.service.system.password.UpdatePasswordRemindLogService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class UpdatePasswordRemindLogServiceImpl extends ServiceImpl<UpdatePasswordRemindLogMapper, UpdatePasswordRemindLog> implements UpdatePasswordRemindLogService {

    private final UpdatePasswordRemindLogRepository updatePasswordRemindLogRepository;

    /**
     * 获取密码到期提醒记录列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 检索关键字
     * @return 密码到期提醒记录列表
     */
    @Override
    public PageData<UpdatePasswordRemindLogResponseDto> list(int page, int size, String keyword) {
        // 1. 查询数据库
        Page<UpdatePasswordRemindLog> pageRequest = new Page<>(page, size);
        updatePasswordRemindLogRepository.searchRemindLogs(pageRequest, keyword);

        // 2. 属性设置
        PageData<UpdatePasswordRemindLogResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var remindLogs = CommonUtil.stream(pageRequest.getRecords()).map(remindLog -> UpdatePasswordRemindLogResponseDto
                .builder()
                .remindTime(remindLog.getRemindTime())
                .remindMethod(remindLog.getRemindMethod())
                .success(remindLog.isSuccess())
                .userId(remindLog.getUser().getUserId())
                .username(remindLog.getUser().getUsername())
                .policyId(remindLog.getPasswordPolicy().getPolicyId())
                .policyName(remindLog.getPasswordPolicy().getPolicyName())
                .build()).toList();
        pageData.setList(remindLogs);
        return pageData;
    }
}
