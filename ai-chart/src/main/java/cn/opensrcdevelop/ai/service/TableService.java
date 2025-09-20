package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.BatchUpdateTableRequestDto;
import cn.opensrcdevelop.ai.dto.TableResponseDto;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface TableService extends IService<Table> {

    PageData<TableResponseDto> list(String dataSourceId, String keyword, int page, int size);

    void batchUpdate(BatchUpdateTableRequestDto requestDto);
}
