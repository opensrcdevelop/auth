package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.DataSourceConfResponseDto;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface DataSourceConfService extends IService<DataSourceConf> {

    PageData<DataSourceConfResponseDto> list(String keyword, int page, int size);
}
