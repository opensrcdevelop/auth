package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.LoginLogResponseDto;
import cn.opensrcdevelop.auth.biz.entity.LoginLog;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import io.vavr.Tuple4;

import java.time.LocalDateTime;

public interface LoginLogService extends IService<LoginLog> {

    void saveLoginLog(String userId, Integer maxLoginLogCnt);

    Tuple4<String, String, String, LocalDateTime> getLastLoginInfo(String userId);

    PageData<LoginLogResponseDto> getUserLoginLogs(int page, int size, String userId);
}
