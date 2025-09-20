package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.BatchUpdateTableRequestDto;
import cn.opensrcdevelop.ai.dto.TableResponseDto;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.mapper.TableMapper;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Service
public class TableServiceImpl extends ServiceImpl<TableMapper, Table> implements TableService {

    /**
     * 获取数据源下的表列表
     *
     * @param dataSourceId 数据源ID
     * @param keyword 表名检索关键字
     * @param page 页数
     * @param size 条数
     * @return 数据源下的表列表
     */
    @Override
    public PageData<TableResponseDto> list(String dataSourceId, String keyword, int page, int size) {
        // 1. 查询数据库
        List<Table> tableList;
        Page<Table> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            tableList = super.list(pageRequest, Wrappers.<Table>lambdaQuery()
                    .eq(Table::getDataSourceId, dataSourceId)
                    .like(Table::getTableName, keyword)
                    .orderByAsc(Table::getTableName));
        } else {
            tableList = super.list(pageRequest, Wrappers.<Table>lambdaQuery()
                    .eq(Table::getDataSourceId, dataSourceId)
                    .orderByAsc(Table::getTableName));
        }

        // 2. 属性编辑
        PageData<TableResponseDto> pageData = new PageData<>();
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());

        List<TableResponseDto> data = CommonUtil.stream(tableList).map(table -> TableResponseDto.builder()
                .id(table.getTableId())
                .name(table.getTableName())
                .remark(table.getRemark())
                .additionalInfo(table.getAdditionalInfo())
                .toUse(table.getToUse()).build())
                .toList();
        pageData.setList(data);
        return pageData;
    }

    /**
     * 批量更新表
     *
     * @param requestDto 请求
     */
    @Override
    public void batchUpdate(BatchUpdateTableRequestDto requestDto) {
        // 1. 属性编辑
        List<Table> updateList = CommonUtil.stream(requestDto.getList()).map(table -> {
            Table updateTable = new Table();
            updateTable.setTableId(table.getId());
            updateTable.setRemark(table.getRemark());
            updateTable.setAdditionalInfo(table.getAdditionalInfo());
            CommonUtil.callSetWithCheck(Objects::nonNull, updateTable::setToUse, table::getToUse);

            return updateTable;
        }).toList();

        // 2. 批量更新
        if (CollectionUtils.isNotEmpty(updateList)) {
            super.updateBatchById(updateList);
        }
    }
}
