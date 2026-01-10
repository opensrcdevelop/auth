package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.BatchUpdateTableRequestDto;
import cn.opensrcdevelop.ai.dto.TableResponseDto;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.mapper.TableMapper;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TableServiceImpl extends ServiceImpl<TableMapper, Table> implements TableService {

    private final TableFieldService tableFieldService;

    /**
     * 获取数据源下的表列表
     *
     * @param dataSourceId
     *            数据源ID
     * @param keyword
     *            表名检索关键字
     * @param page
     *            页数
     * @param size
     *            条数
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
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI_DATA_SOURCE, sysOperation = SysOperationType.UPDATE, success = "批量更新了数据源中的表信息", fail = "批量更新数据源中的表信息失败")
    @Override
    public void batchUpdate(BatchUpdateTableRequestDto requestDto) {
        // 1. 属性编辑
        List<Table> updateList = CommonUtil.stream(requestDto.getList()).map(table -> {
            Table updateTable = new Table();
            updateTable.setTableId(table.getId());
            updateTable.setRemark(table.getRemark());
            updateTable.setAdditionalInfo(table.getAdditionalInfo());
            CommonUtil.callSetWithCheck(Objects::nonNull, updateTable::setToUse, table::getToUse);

            // 审计比较对象
            AuditContext.addCompareObj(CompareObj.builder()
                    .id(updateTable.getTableId())
                    .before(super.getById(updateTable.getTableId()))
                    .after(updateTable)
                    .build());

            return updateTable;
        }).toList();

        // 2. 批量更新
        if (CollectionUtils.isNotEmpty(updateList)) {
            super.updateBatchById(updateList);
        }
    }

    /**
     * 删除数据源下的所有表
     *
     * @param dataSourceId
     *            数据源ID
     */
    @Transactional
    @Override
    public void removeTables(String dataSourceId) {
        // 1. 查询数据源下的所有表ID
        List<String> tableIds = CommonUtil.stream(super.list(Wrappers.<Table>lambdaQuery()
                .select(Table::getTableId)
                .eq(Table::getDataSourceId, dataSourceId))).map(Table::getTableId).toList();

        // 2. 删除表及字段
        if (CollectionUtils.isNotEmpty(tableIds)) {
            super.remove(Wrappers.<Table>lambdaQuery().in(Table::getTableId, tableIds));
            tableFieldService.removeTableFields(tableIds);
        }
    }

    /**
     * 获取 Table Schema
     *
     * @param tables
     *            表列表
     * @return Table Schema
     */
    @Override
    public List<Map<String, Object>> getTableSchemas(List<Map<String, Object>> tables) {
        List<String> tableIds = CommonUtil.stream(tables).map(x -> x.get("table_id").toString()).toList();
        List<TableField> allTableFields = tableFieldService.list(Wrappers.<TableField>lambdaQuery()
                .in(TableField::getTableId, tableIds));

        return CommonUtil.stream(tables).map(table -> {
            Map<String, Object> newTableInfo = new HashMap<>(table);
            List<TableField> tableFields = CommonUtil.stream(allTableFields)
                    .filter(x -> x.getTableId().equals(table.get("table_id"))).toList();
            List<String> fieldDescriptions = CommonUtil.stream(tableFields).map(x -> {
                Map<String, String> fieldDescription = new HashMap<>();
                fieldDescription.put("field_name", x.getFieldName());
                fieldDescription.put("field_data_type", x.getFieldType());
                fieldDescription.put("description", x.getRemark() == null ? "No description available" : x.getRemark());
                fieldDescription.put("additional_info",
                        x.getAdditionalInfo() == null ? "No additional info available" : x.getAdditionalInfo());
                return CommonUtil.serializeObject(fieldDescription);
            }).toList();
            newTableInfo.put("fields", fieldDescriptions);
            return newTableInfo;
        }).toList();
    }

    /**
     * 获取数据源下的所有表
     *
     * @param dataSourceId
     *            数据源ID
     * @return 数据源下的所有表
     */
    @Override
    public List<Map<String, Object>> getTables(String dataSourceId) {
        List<Table> tables = super.list(Wrappers.<Table>lambdaQuery()
                .eq(Table::getDataSourceId, dataSourceId)
                .eq(Table::getToUse, true));

        return CommonUtil.stream(tables).map(table -> {
            Map<String, Object> tableDescription = new HashMap<>();
            tableDescription.put("table_id", table.getTableId());
            tableDescription.put("table_name", table.getTableName());
            tableDescription.put("description",
                    table.getRemark() == null ? "No description available" : table.getRemark());
            tableDescription.put("additional_info",
                    table.getAdditionalInfo() == null ? "No additional info available" : table.getAdditionalInfo());
            return tableDescription;
        }).toList();
    }

    /**
     * 获取表的禁止字段
     *
     * @param tableId
     *            表ID
     * @return 表的禁止字段
     */
    @Override
    public List<String> getTableForbiddenFields(String tableId) {
        List<TableField> forbiddenFields = tableFieldService.list(Wrappers.<TableField>lambdaQuery()
                .select(TableField::getFieldName)
                .eq(TableField::getTableId, tableId)
                .eq(TableField::getToUse, false));
        return CommonUtil.stream(forbiddenFields).map(TableField::getFieldName).toList();
    }
}
