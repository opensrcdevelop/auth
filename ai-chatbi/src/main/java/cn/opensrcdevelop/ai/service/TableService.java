package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.BatchUpdateTableRequestDto;
import cn.opensrcdevelop.ai.dto.TableResponseDto;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Map;

public interface TableService extends IService<Table> {

    PageData<TableResponseDto> list(String dataSourceId, String keyword, int page, int size);

    void batchUpdate(BatchUpdateTableRequestDto requestDto);

    void removeTables(String dataSourceId);

    List<Map<String, Object>> getTableSchemas(List<Map<String, Object>> tables);

    List<Map<String, Object>> getTables(String dataSourceId);

    List<String> getTableForbiddenFields(String tableId);

    List<Map<String, Object>> getTableFields(String tableId);
}
