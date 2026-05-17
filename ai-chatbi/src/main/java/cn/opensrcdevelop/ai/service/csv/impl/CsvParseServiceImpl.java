package cn.opensrcdevelop.ai.service.csv.impl;

import cn.opensrcdevelop.ai.converter.TableFieldTypeConverter;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.enums.TableFieldType;
import cn.opensrcdevelop.ai.service.csv.CsvParseService;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * CSV 解析服务实现
 * <p>
 * 使用 DuckDB read_csv_auto() 推断 CSV 表结构，并将解析结果存储到 t_table 和 t_table_field
 * </p>
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class CsvParseServiceImpl implements CsvParseService {

    private final DataSourceManager dataSourceManager;
    private final TableFieldTypeConverter tableFieldTypeConverter;

    @Value("${csv-ds.storage.s3.bucket:}")
    private String s3Bucket;

    @Override
    public List<TableField> parseCsvSchema(String dataSourceId, String tableName, String s3Path) {
        List<TableField> fields = new ArrayList<>();

        // S3 URI 格式: s3://{bucket}/{dataSourceId}/{tableName}.csv
        String s3Uri = "s3://" + s3Bucket + "/" + s3Path;
        String sql = "DESCRIBE SELECT * FROM read_csv_auto('" + escapeString(s3Uri) + "')";

        log.info("解析 CSV 表结构: dataSourceId={}, tableName={}, s3Uri={}", dataSourceId, tableName, s3Uri);

        try (Connection conn = dataSourceManager.getConnection(dataSourceId);
                Statement stmt = conn.createStatement();
                ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                String columnName = rs.getString("column_name");
                String columnType = rs.getString("column_type");

                // 双引号转义
                String escapedColumnName = "\"" + escapeString(columnName) + "\"";

                // 类型映射
                TableFieldType mappedType = tableFieldTypeConverter.convert(DataSourceType.DUCKDB, columnType);

                TableField field = new TableField();
                field.setFieldId(CommonUtil.getUUIDV7String());
                field.setFieldName(escapedColumnName);
                field.setFieldType(mappedType.name());
                field.setToUse(true);

                fields.add(field);
            }

            log.info("CSV 表结构解析完成: {} 个字段", fields.size());

        } catch (Exception e) {
            log.error("CSV 表结构解析失败: dataSourceId={}, tableName={}", dataSourceId, tableName, e);
            throw new ServerException("CSV 表结构解析失败: " + e.getMessage(), e);
        }

        return fields;
    }

    /**
     * 转义 SQL 字符串中的特殊字符
     */
    private String escapeString(String input) {
        if (input == null) {
            return "";
        }
        return input.replace("\\", "\\\\").replace("'", "\\'");
    }
}
