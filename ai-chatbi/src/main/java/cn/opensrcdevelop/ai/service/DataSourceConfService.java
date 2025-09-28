package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.DataSourceConfRequestDto;
import cn.opensrcdevelop.ai.dto.DataSourceConfResponseDto;
import cn.opensrcdevelop.ai.dto.TestDataSourceConnRequestDto;
import cn.opensrcdevelop.ai.dto.TestDataSourceConnResponseDto;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface DataSourceConfService extends IService<DataSourceConf> {

    List<DataSourceConfResponseDto> enabledList();

    PageData<DataSourceConfResponseDto> list(String keyword, int page, int size);

    DataSourceConfResponseDto detail(String id);

    void syncTable(String id);

    void createDataSourceConf(DataSourceConfRequestDto requestDto);

    void updateDataSourceConf(DataSourceConfRequestDto requestDto);

    TestDataSourceConnResponseDto testConn(TestDataSourceConnRequestDto requestDto);

    void removeDataSourceConf(String id);

    boolean isSynced(String id);
}
