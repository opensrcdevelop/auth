package cn.opensrcdevelop.ai.service.csv;

import cn.opensrcdevelop.ai.entity.TableField;
import java.util.List;

/**
 * CSV 解析服务接口
 * <p>
 * 用于解析 CSV 文件的表结构，使用 DuckDB read_csv_auto() 自动推断列类型
 * </p>
 */
public interface CsvParseService {

    /**
     * 解析 CSV 表结构
     *
     * @param dataSourceId
     *            数据源ID
     * @param tableName
     *            表名
     * @param s3Path
     *            S3 路径（csv-datasource/{dataSourceId}/{fileName}.csv）
     * @return 字段列表
     */
    List<TableField> parseCsvSchema(String dataSourceId, String tableName, String s3Path);
}
