package cn.opensrcdevelop.ai.datasource;

import cn.opensrcdevelop.ai.converter.TableFieldTypeConverter;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.ai.service.csv.CsvDatasourceStorageService;
import cn.opensrcdevelop.ai.service.csv.CsvParseService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.annotation.Resource;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Component
public class DataSourceMetaCollector {

    private final TableService tableService;
    private final TableFieldService tableFieldService;
    private final TableFieldTypeConverter tableFieldTypeConverter;

    @Resource
    @Lazy
    private DataSourceConfService dataSourceConfService;

    @Resource
    @Lazy
    private DataSourceManager dataSourceManager;

    @Resource
    @Lazy
    private CsvDatasourceStorageService csvStorageService;

    @Resource
    @Lazy
    private CsvParseService csvParseService;

    public DataSourceMetaCollector(TableService tableService,
            TableFieldService tableFieldService,
            @Lazy TableFieldTypeConverter tableFieldTypeConverter,
            @Lazy DataSourceManager dataSourceManager,
            @Lazy CsvDatasourceStorageService csvStorageService,
            @Lazy CsvParseService csvParseService) {
        this.tableService = tableService;
        this.tableFieldService = tableFieldService;
        this.tableFieldTypeConverter = tableFieldTypeConverter;
        this.dataSourceManager = dataSourceManager;
        this.csvStorageService = csvStorageService;
        this.csvParseService = csvParseService;
    }

    /**
     * 收集数据源元信息
     *
     * @param dataSourceId
     *            数据源ID
     */
    @Transactional
    @SuppressWarnings("java:S3776")
    public void collect(String dataSourceId) {
        // 1. 获取数据源配置
        DataSourceConf dataSourceConf = dataSourceConfService.getById(dataSourceId);

        // 2. DuckDB 类型特殊处理：从 S3 获取 CSV 文件列表同步
        if (DataSourceType.DUCKDB.name().equals(dataSourceConf.getDataSourceType())) {
            collectDuckDbCsvTables(dataSourceId, dataSourceConf);
            return;
        }

        // 3. 其他数据源：使用 JDBC DatabaseMetaData 获取表信息
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

            for (Table table : tables) {
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
                List<TableField> tableFields = getTableFields(databaseMetaData,
                        DataSourceType.valueOf(dataSourceConf.getDataSourceType()), table);

                // 7. 删除不存在的表字段信息
                List<String> deleteTableFieldIds = CommonUtil.stream(existTableFields)
                        .filter(tableField -> tableFields.stream()
                                .noneMatch(t -> t.getFieldName().equals(tableField.getFieldName())))
                        .map(TableField::getFieldId)
                        .toList();
                if (CollectionUtils.isNotEmpty(deleteTableFieldIds)) {
                    tableFieldService.removeByIds(deleteTableFieldIds);
                }

                for (TableField tableField : tableFields) {
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
            updateDataSourceConf.setSyncTableCount(
                    dataSourceConf.getSyncTableCount() == null ? 1 : dataSourceConf.getSyncTableCount() + 1);
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
                new String[]{"TABLE"})) {
            while (rs.next()) {
                Table table = new Table();
                table.setTableName(rs.getString("TABLE_NAME"));
                table.setRemark(rs.getString("REMARKS"));
                tables.add(table);
            }
        }
        return tables;
    }

    private List<TableField> getTableFields(DatabaseMetaData databaseMetaData, DataSourceType dataSourceType,
            Table table) throws SQLException {
        List<TableField> tableFields = new ArrayList<>();

        try (ResultSet rs = databaseMetaData.getColumns(
                null,
                null,
                table.getTableName(),
                "%")) {
            while (rs.next()) {
                TableField tableField = new TableField();
                tableField.setFieldName(rs.getString("COLUMN_NAME"));
                tableField.setFieldType(
                        tableFieldTypeConverter.convert(dataSourceType, rs.getString("TYPE_NAME")).name());
                tableField.setRemark(rs.getString("REMARKS"));
                tableFields.add(tableField);
            }
        }

        return tableFields;
    }

    /**
     * 收集 DuckDB CSV 表信息
     * <p>
     * 从 S3 存储桶中获取指定 dataSourceId 下的所有 CSV 文件，解析表结构并同步到数据库
     * </p>
     *
     * @param dataSourceId
     *            数据源ID
     * @param dataSourceConf
     *            数据源配置
     */
    private void collectDuckDbCsvTables(String dataSourceId, DataSourceConf dataSourceConf) {
        // 1. 获取 S3 中该数据源的所有 CSV 文件
        String prefix = dataSourceId + CommonConstants.SLASH;
        List<String> csvFiles = csvStorageService.list(prefix);

        // 2. 获取已有的 CSV 表信息
        List<Table> existTables = tableService.list(Wrappers.<Table>lambdaQuery()
                .eq(Table::getDataSourceId, dataSourceId));

        // 3. 提取 CSV 文件名（去掉路径和 .csv 后缀）
        List<String> csvTableNames = csvFiles.stream()
                .map(path -> {
                    String fileName = path.substring(path.lastIndexOf('/') + 1);
                    return fileName.replaceAll("\\.csv$", "");
                })
                .toList();

        // 4. 删除 S3 中不再存在的表信息
        List<String> deleteTableIds = existTables.stream()
                .filter(table -> !csvTableNames.contains(table.getTableName()))
                .map(Table::getTableId)
                .toList();
        if (CollectionUtils.isNotEmpty(deleteTableIds)) {
            tableService.removeBatchByIds(deleteTableIds);
        }

        // 5. 遍历 S3 中的 CSV 文件，同步表结构
        for (String csvFile : csvFiles) {
            // 提取表名
            String tableName = csvFile.substring(csvFile.lastIndexOf('/') + 1).replaceAll("\\.csv$", "");

            // 5.1 检查表是否存在，不存在则创建
            Table tmpTable = tableService.getOne(Wrappers.<Table>lambdaQuery()
                    .eq(Table::getTableName, tableName)
                    .eq(Table::getDataSourceId, dataSourceId));

            String tableId;
            if (Objects.isNull(tmpTable)) {
                tableId = CommonUtil.getUUIDV7String();
                Table table = new Table();
                table.setTableId(tableId);
                table.setDataSourceId(dataSourceId);
                table.setTableName(tableName);
                table.setToUse(true);
                tableService.save(table);
            } else {
                tableId = tmpTable.getTableId();
            }

            // 5.2 解析并同步 CSV 字段
            syncCsvFields(dataSourceId, tableId, csvFile);
        }

        // 6. 更新数据源同步信息
        DataSourceConf updateDataSourceConf = new DataSourceConf();
        updateDataSourceConf.setDataSourceId(dataSourceId);
        updateDataSourceConf.setLastSyncTableTime(LocalDateTime.now());
        Long syncTableCount = dataSourceConf.getSyncTableCount();
        updateDataSourceConf.setSyncTableCount(syncTableCount == null ? 1 : syncTableCount + 1);
        dataSourceConfService.updateById(updateDataSourceConf);

        log.info("DuckDB CSV 表同步完成: dataSourceId={}, csvFileCount={}", dataSourceId, csvFiles.size());
    }

    /**
     * 同步 CSV 字段
     * <p>
     * 复用 CsvParseAsyncTaskExecutor 的同步策略：删除不存在的字段，跳过已存在的，新增新的
     * </p>
     *
     * @param dataSourceId
     *            数据源ID
     * @param tableId
     *            表ID
     * @param csvFile
     *            CSV 文件路径
     */
    private void syncCsvFields(String dataSourceId, String tableId, String csvFile) {
        // 提取表名
        String tableName = csvFile.substring(csvFile.lastIndexOf('/') + 1).replaceAll("\\.csv$", "");

        // 解析 CSV 表结构
        List<TableField> newFields = csvParseService.parseCsvSchema(dataSourceId, tableName, csvFile);

        // 获取现有字段列表
        List<TableField> existingFields = tableFieldService.list(
                Wrappers.<TableField>lambdaQuery()
                        .eq(TableField::getTableId, tableId));

        // 删除 CSV 中不再存在的字段
        List<String> deleteFieldIds = existingFields.stream()
                .filter(ef -> newFields.stream()
                        .noneMatch(f -> f.getFieldName().equals(ef.getFieldName())))
                .map(TableField::getFieldId)
                .toList();
        if (CollectionUtils.isNotEmpty(deleteFieldIds)) {
            tableFieldService.removeByIds(deleteFieldIds);
        }

        // 跳过已存在的字段（同名不处理）
        final List<String> finalExistFieldNames = existingFields.stream()
                .map(TableField::getFieldName)
                .toList();
        List<TableField> fieldsToAdd = newFields.stream()
                .filter(f -> !finalExistFieldNames.contains(f.getFieldName()))
                .toList();

        // 如果没有新字段需要添加，直接返回
        if (fieldsToAdd.isEmpty()) {
            return;
        }

        // 保存新增的字段记录
        for (TableField field : fieldsToAdd) {
            field.setTableId(tableId);
            field.setToUse(true);
        }
        tableFieldService.saveBatch(fieldsToAdd);
    }
}
