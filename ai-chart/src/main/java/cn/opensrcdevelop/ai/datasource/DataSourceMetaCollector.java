package cn.opensrcdevelop.ai.datasource;

import cn.opensrcdevelop.ai.converter.TableFieldTypeConverter;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
@Component
@RequiredArgsConstructor
public class DataSourceMetaCollector {

    private final TableService tableService;
    private final TableFieldService tableFieldService;
    private final DataSourceManager dataSourceManager;
    private final TableFieldTypeConverter tableFieldTypeConverter;

    @Resource
    @Lazy
    private DataSourceConfService dataSourceConfService;

    /**
     * 收集数据源元信息
     *
     * @param dataSourceId 数据源ID
     */
    @Transactional
    public void collect(String dataSourceId) {
        // 1. 获取数据源配置
        DataSourceConf dataSourceConf = dataSourceConfService.getById(dataSourceId);

        // 2. 获取数据库连接
        try (Connection connection = dataSourceManager.getConnection(dataSourceId)) {
            DatabaseMetaData databaseMetaData = connection.getMetaData();
            // 3. 获取表信息
            // 3.1 获取已有的表信息
            List<Table> existTables = tableService.list(Wrappers.<Table>lambdaQuery()
                    .eq(Table::getDataSourceId, dataSourceId));
            // 3.2 获取最新的表信息
            List<Table> tables = getTables(databaseMetaData);

            // 4. 删除不存在的表信息
            List<String> deleteTableIds = CommonUtil.stream(existTables)
                    .filter(table -> tables.stream().noneMatch(t -> t.getTableName().equals(table.getTableName())))
                    .map(Table::getTableId)
                    .toList();
            if (CollectionUtils.isNotEmpty(deleteTableIds)) {
                tableService.removeBatchByIds(deleteTableIds);
            }

            for (Table table: tables) {
                // 5. 添加或更新表信息
                Table tmpTable = tableService.getOne(Wrappers.<Table>lambdaQuery()
                        .eq(Table::getTableName, table.getTableName())
                        .eq(Table::getDataSourceId, dataSourceId));

                String tableId;
                // 5.1 表不存在，添加表信息
                if (Objects.isNull(tmpTable)) {
                    tableId = CommonUtil.getUUIDV7String();
                    table.setDataSourceId(dataSourceId);
                    table.setTableId(tableId);
                    table.setToUse(true);
                    tableService.save(table);
                } else {
                    // 5.2 表存在，更新表信息
                    tableId = tmpTable.getTableId();
                    table.setTableId(tableId);
                    tableService.updateById(table);
                }

                // 6. 获取表字段信息
                // 6.1 获取已有的表字段信息
                List<TableField> existTableFields = tableFieldService.list(Wrappers.<TableField>lambdaQuery()
                       .eq(TableField::getTableId, tableId));
                // 6.2 获取最新的表字段信息
                List<TableField> tableFields = getTableFields(databaseMetaData, DataSourceType.valueOf(dataSourceConf.getDataSourceType()), table);

                // 7. 删除不存在的表字段信息
                List<String> deleteTableFieldIds = CommonUtil.stream(existTableFields)
                       .filter(tableField -> tableFields.stream().noneMatch(t -> t.getFieldName().equals(tableField.getFieldName())))
                       .map(TableField::getFieldId)
                       .toList();
                if (CollectionUtils.isNotEmpty(deleteTableFieldIds)) {
                    tableFieldService.removeByIds(deleteTableFieldIds);
                }

                for (TableField tableField: tableFields) {
                    // 8. 添加或更新表字段信息
                    TableField tmpTableField = tableFieldService.getOne(Wrappers.<TableField>lambdaQuery()
                            .eq(TableField::getFieldName, tableField.getFieldName())
                            .eq(TableField::getTableId, tableId));

                    // 8.1 字段不存在，添加字段信息
                    if (Objects.isNull(tmpTableField)) {
                        tableField.setTableId(tableId);
                        tableField.setFieldId(CommonUtil.getUUIDV7String());
                        tableField.setToUse(true);
                        tableFieldService.save(tableField);
                    } else {
                        // 8.2 字段存在，更新字段信息
                        tableField.setFieldId(tmpTableField.getFieldId());
                        tableFieldService.updateById(tableField);
                    }
                }
            }

            // 9. 更新数据源同步信息
            DataSourceConf updateDataSourceConf = new DataSourceConf();
            updateDataSourceConf.setDataSourceId(dataSourceId);
            updateDataSourceConf.setLastSyncTableTime(LocalDateTime.now());
            updateDataSourceConf.setSyncTableCount(dataSourceConf.getSyncTableCount() == null ? 1 : dataSourceConf.getSyncTableCount() + 1);
            dataSourceConfService.updateById(updateDataSourceConf);
        } catch (SQLException ex) {
            log.error("收集数据库元信息失败，数据源ID：{}", dataSourceId);
            throw new ServerException(ex);
        }
    }

    private List<Table> getTables(DatabaseMetaData databaseMetaData) throws SQLException {
        List<Table> tables = new ArrayList<>();
        try (ResultSet rs = databaseMetaData.getTables(
                null,
                null,
                "%",
                new String[]{"TABLE"}
        )) {
            while (rs.next()) {
                Table table = new Table();
                table.setTableName(rs.getString("TABLE_NAME"));
                table.setRemark(rs.getString("REMARKS"));
                tables.add(table);
            }
        }
        return tables;
    }

    private List<TableField> getTableFields(DatabaseMetaData databaseMetaData, DataSourceType dataSourceType, Table table) throws SQLException {
        List<TableField> tableFields = new ArrayList<>();

        try (ResultSet rs = databaseMetaData.getColumns(
                null,
                null,
                table.getTableName(),
                "%"
        )) {
            while (rs.next()) {
                TableField tableField = new TableField();
                tableField.setFieldName(rs.getString("COLUMN_NAME"));
                tableField.setFieldType(tableFieldTypeConverter.convert(dataSourceType, rs.getString("TYPE_NAME")).name());
                tableField.setRemark(rs.getString("REMARKS"));
                tableFields.add(tableField);
            }
        }

        return tableFields;
    }
}
