package cn.opensrcdevelop.ai.converter;

import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class DataSourcePromptInfoConverter {

    private final TableService tableService;
    private final TableFieldService tableFieldService;

    /**
     * 将数据库表信息转换为 Prompt JSON 格式
     *
     * @param dataSourceId 数据源ID
     * @return Prompt JSON 格式的表信息
     */
    public String convert(String dataSourceId) {
        // 1. 获取关联的全部表信息
        List<Table> tables = tableService.list(Wrappers.<Table>lambdaQuery()
                .eq(Table::getDataSourceId, dataSourceId));
        if (CollectionUtils.isEmpty(tables)) {
            return StringUtils.EMPTY;
        }

        // 2. 获取表中的字段信息
        List<TableField> tableFields = tableFieldService.list(Wrappers.<TableField>lambdaQuery()
                .in(TableField::getTableId, tables.stream().map(Table::getTableId).toList())
                .eq(TableField::getToUse, true));

        // 3. 转换为 Prompt JSON 格式
        Map<String, Object> promptMap = new HashMap<>();
        List<Map<String, String>> tableInfos = new ArrayList<>();
        for (Table table : tables) {
            Map<String, String> tableInfo = new HashMap<>();
            tableInfo.put("表名", table.getTableName());
            tableInfo.put("表描述", StringUtils.isNotBlank(table.getRemark()) ? table.getRemark() : "无");
            tableInfo.put("补充信息", StringUtils.isNotBlank(table.getAdditionalInfo()) ? table.getAdditionalInfo() : "无");
            setTableFieldInfo(tableFields, table.getTableId(), tableInfo);
            tableInfos.add(tableInfo);
        }
        promptMap.put("表信息", CommonUtil.serializeObject(tableInfos));
        return CommonUtil.serializeObject(promptMap);
    }

    private void setTableFieldInfo(List<TableField> tableFields, String tableId, Map<String, String> tableInfo) {
        List<TableField> fields = CommonUtil.stream(tableFields)
                .filter(field -> field.getTableId().equals(tableId))
                .toList();
        if (CollectionUtils.isEmpty(fields)) {
            tableInfo.put("字段信息", "无");
        } else {
            for (TableField field : fields) {
                Map<String, String> fieldInfo = new HashMap<>();
                fieldInfo.put("字段名", field.getFieldName());
                fieldInfo.put("字段数据类型", field.getFieldType());
                fieldInfo.put("字段描述", StringUtils.isNotBlank(field.getRemark()) ? field.getRemark() : "无");
                fieldInfo.put("补充信息", StringUtils.isNotBlank(field.getAdditionalInfo())? field.getAdditionalInfo() : "无");

                tableInfo.put("字段信息", CommonUtil.serializeObject(fieldInfo));
            }
        }
    }
}
