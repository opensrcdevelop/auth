package cn.opensrcdevelop.auth.biz.util.excel;

import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.UserExcelImportDto;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.metadata.CellExtra;
import com.alibaba.excel.metadata.data.ReadCellData;
import com.alibaba.excel.read.listener.ReadListener;
import com.alibaba.excel.read.metadata.ReadSheet;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

/**
 * Excel 导入读取处理器 - 处理中文标题映射
 */
@Slf4j
public class UserExcelImportHandler implements ReadListener<Map<Integer, String>> {

    private final List<UserExcelImportDto> dataList = new ArrayList<>();

    private final Map<String, UserAttrResponseDto> attrMap;

    private final Map<String, Map<String, String>> dictValueToIdMap;

    /** 中文标题 -> 字段 key 的映射 */
    private Map<String, String> headerToKeyMap = new HashMap<>();

    /** 列索引 -> 字段 key 的映射 */
    private Map<Integer, String> headerIndexToKeyMap = new LinkedHashMap<>();

    public UserExcelImportHandler(Map<String, UserAttrResponseDto> attrMap,
            Map<String, Map<String, String>> dictValueToIdMap,
            InputStream excelStream) {
        this.attrMap = attrMap;
        this.dictValueToIdMap = dictValueToIdMap;
        // 从隐藏 sheet 读取中文标题到字段 key 的映射
        readHeaderMappingFromHiddenSheet(excelStream);
    }

    /**
     * 从隐藏 sheet 读取中文标题到字段 key 的映射
     */
    private void readHeaderMappingFromHiddenSheet(InputStream excelStream) {
        try (XSSFWorkbook workbook = new XSSFWorkbook(excelStream)) {
            Sheet hiddenSheet = workbook.getSheet("_field_mapping");
            if (hiddenSheet == null) {
                log.warn("未找到隐藏 sheet '_field_mapping'，使用备用映射方式");
                buildFallbackMapping();
                return;
            }

            // 读取字段映射（中文标题 -> 字段 key）
            for (int i = 0; i <= hiddenSheet.getLastRowNum(); i++) {
                Row row = hiddenSheet.getRow(i);
                if (row == null) {
                    continue;
                }

                Cell chineseTitleCell = row.getCell(0);
                Cell fieldKeyCell = row.getCell(1);

                if (chineseTitleCell != null && fieldKeyCell != null) {
                    String chineseTitle = chineseTitleCell.getStringCellValue();
                    String fieldKey = fieldKeyCell.getStringCellValue();

                    // 跳过表头行和非映射数据
                    if ("中文标题".equals(chineseTitle) || chineseTitle.isEmpty()) {
                        continue;
                    }

                    // 跳过字典数据区域（从"字典ID"开始是字典数据）
                    if ("字典ID".equals(chineseTitle)) {
                        break;
                    }

                    if (!chineseTitle.isEmpty() && !fieldKey.isEmpty()) {
                        headerToKeyMap.put(chineseTitle, fieldKey);
                        log.debug("映射建立: '{}' -> '{}'", chineseTitle, fieldKey);
                    }
                }
            }

            log.info("从隐藏 sheet 读取到 {} 个字段映射", headerToKeyMap.size());
        } catch (Exception e) {
            log.error("读取隐藏 sheet 失败，使用备用映射方式", e);
            buildFallbackMapping();
        }
    }

    /**
     * 构建备用映射（直接从 attrMap 构建）
     */
    private void buildFallbackMapping() {
        for (Map.Entry<String, UserAttrResponseDto> entry : attrMap.entrySet()) {
            UserAttrResponseDto attr = entry.getValue();
            if (attr.getName() != null) {
                headerToKeyMap.put(attr.getName(), entry.getKey());
            }
        }
        // 添加操作类型映射
        headerToKeyMap.put("操作类型*", "operationType");
        log.info("备用映射建立: {} 个字段", headerToKeyMap.size());
    }

    @Override
    public void invoke(Map<Integer, String> rowData, AnalysisContext context) {
        int rowIndex = context.readRowHolder().getRowIndex();

        // 第2行是示例行，建立中文标题到列索引的映射
        if (rowIndex == 2) {
            buildHeaderMapping(rowData);
            return;
        }

        // 第3行开始是数据行
        if (rowIndex >= 3) {
            UserExcelImportDto dto = convertToDto(rowData);
            dataList.add(dto);
        }
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext context) {
        log.info("Excel 读取完成，共 {} 条数据", dataList.size());
    }

    @Override
    public void onException(Exception exception, AnalysisContext context) throws Exception {
        log.error("读取 Excel 数据异常", exception);
        throw exception;
    }

    @Override
    public void extra(CellExtra extra, AnalysisContext context) {
        // 处理合并单元格
    }

    /**
     * 建立中文标题到列索引的映射（从示例行读取中文标题，然后通过 headerToKeyMap 转换为字段 key）
     */
    private void buildHeaderMapping(Map<Integer, String> rowData) {
        // 从示例行读取中文标题，建立列索引到字段 key 的映射
        for (Map.Entry<Integer, String> cellEntry : rowData.entrySet()) {
            Integer colIndex = cellEntry.getKey();
            String chineseTitle = cellEntry.getValue();

            // 从 headerToKeyMap 中查找对应的字段 key
            String fieldKey = headerToKeyMap.get(chineseTitle);
            if (fieldKey != null) {
                headerIndexToKeyMap.put(colIndex, fieldKey);
                log.debug("列 {} -> 字段 '{}' (中文: '{}')", colIndex, fieldKey, chineseTitle);
            }
        }

        log.info("标题映射已建立: {}", headerIndexToKeyMap);
    }

    /**
     * 将行数据转换为 DTO
     */
    private UserExcelImportDto convertToDto(Map<Integer, String> rowData) {
        UserExcelImportDto dto = new UserExcelImportDto();
        Map<String, Object> extAttrs = new HashMap<>();

        for (Map.Entry<Integer, String> cellEntry : rowData.entrySet()) {
            Integer colIndex = cellEntry.getKey();
            String value = cellEntry.getValue();
            String fieldKey = headerIndexToKeyMap.get(colIndex);

            if (fieldKey == null) {
                continue;
            }

            // 根据字段 key 设置值
            switch (fieldKey) {
                case "operationType" :
                    dto.setOperationType(parseInteger(value));
                    break;
                case "username" :
                    dto.setUsername(value);
                    break;
                case "emailAddress" :
                    dto.setEmailAddress(value);
                    break;
                case "phoneNumber" :
                    dto.setPhoneNumber(value);
                    break;
                case "locked" :
                    dto.setLocked(parseBoolean(value));
                    break;
                case "consoleAccess" :
                    dto.setConsoleAccess(parseBoolean(value));
                    break;
                case "enableMfa" :
                    dto.setEnableMfa(parseBoolean(value));
                    break;
                default :
                    // 扩展属性
                    UserAttrResponseDto attr = attrMap.get(fieldKey);
                    if (attr != null && Boolean.TRUE.equals(attr.getExtFlg())) {
                        String processedValue = value;

                        // 字典类型：转换显示文本到数据 ID
                        if ("DICT".equals(attr.getDataType()) && attr.getDictId() != null) {
                            Map<String, String> valueToIdMap = dictValueToIdMap.get(fieldKey);
                            if (valueToIdMap != null && valueToIdMap.containsKey(value)) {
                                processedValue = valueToIdMap.get(value);
                            }
                        }

                        // 日期类型：转换为时间戳
                        if ("DATE".equals(attr.getDataType())) {
                            processedValue = convertDateToTimestamp(value);
                        } else if ("DATETIME".equals(attr.getDataType())) {
                            processedValue = convertDateTimeToTimestamp(value);
                        }

                        extAttrs.put(fieldKey, processedValue);
                    }
                    break;
            }
        }

        dto.setExtAttrs(extAttrs);
        return dto;
    }

    private Integer parseInteger(String value) {
        if (value == null || value.isEmpty()) {
            return null;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private Boolean parseBoolean(String value) {
        if (value == null || value.isEmpty()) {
            return null;
        }
        return "是".equals(value) || "true".equalsIgnoreCase(value) || "1".equals(value);
    }

    /**
     * 日期转时间戳（格式：yyyyMMdd）
     */
    private String convertDateToTimestamp(String dateStr) {
        if (dateStr == null || dateStr.length() != 8) {
            return dateStr;
        }
        try {
            int year = Integer.parseInt(dateStr.substring(0, 4));
            int month = Integer.parseInt(dateStr.substring(4, 6)) - 1;
            int day = Integer.parseInt(dateStr.substring(6, 8));
            java.time.LocalDate date = java.time.LocalDate.of(year, month, day);
            return String.valueOf(date.atStartOfDay(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli());
        } catch (Exception e) {
            return dateStr;
        }
    }

    /**
     * 日期时间转时间戳（格式：yyyyMMddHHmmss）
     */
    private String convertDateTimeToTimestamp(String dateTimeStr) {
        if (dateTimeStr == null || dateTimeStr.length() != 14) {
            return dateTimeStr;
        }
        try {
            int year = Integer.parseInt(dateTimeStr.substring(0, 4));
            int month = Integer.parseInt(dateTimeStr.substring(4, 6)) - 1;
            int day = Integer.parseInt(dateTimeStr.substring(6, 8));
            int hour = Integer.parseInt(dateTimeStr.substring(8, 10));
            int minute = Integer.parseInt(dateTimeStr.substring(10, 12));
            int second = Integer.parseInt(dateTimeStr.substring(12, 14));
            java.time.LocalDateTime dateTime = java.time.LocalDateTime.of(year, month, day, hour, minute, second);
            return String.valueOf(dateTime.atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli());
        } catch (Exception e) {
            return dateTimeStr;
        }
    }

    public List<UserExcelImportDto> getDataList() {
        return dataList;
    }
}
