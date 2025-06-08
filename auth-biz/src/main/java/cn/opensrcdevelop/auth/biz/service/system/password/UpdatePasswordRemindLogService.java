package cn.opensrcdevelop.auth.biz.service.system.password;

import cn.opensrcdevelop.auth.biz.dto.system.password.UpdatePasswordRemindLogResponseDto;
import cn.opensrcdevelop.auth.biz.entity.system.password.UpdatePasswordRemindLog;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface UpdatePasswordRemindLogService extends IService<UpdatePasswordRemindLog> {

    PageData<UpdatePasswordRemindLogResponseDto> list(int page, int size, String keyword);
}
