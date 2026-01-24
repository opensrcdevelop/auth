package cn.opensrcdevelop.auth.biz.util.excel;

import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.UserExcelImportDto;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.metadata.CellExtra;
import com.alibaba.excel.read.listener.ReadListener;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.SheetVisibility;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * Excel 导入读取处理器 - 处理中文标题映射
 */
@Slf4j
public class UserExcelImportHandler implements ReadListener<Map<Integer, String>> {

    @Getter
    private final List<UserExcelImportDto> dataList = new ArrayList<>();

    private final Map<String, UserAttrResponseDto> attrMap;

    private final Map<String, Map<String, String>> dictValueToIdMap;

    /** 中文标题 -> 字段 key 的映射 */
    private final Map<String, String> headerToKeyMap = new HashMap<>();

    /** 字段 key -> 中文标题的反向映射（用于错误显示） */
    @Getter
    private final Map<String, String> keyToHeaderMap = new HashMap<>();

    /** 列索引 -> 字段 key 的映射 */
    private final Map<Integer, String> headerIndexToKeyMap = new LinkedHashMap<>();

    /** Excel 文件字节数组副本（用于多次读取） */
    private final byte[] excelBytes;

    public UserExcelImportHandler(Map<String, UserAttrResponseDto> attrMap,
            Map<String, Map<String, String>> dictValueToIdMap,
            InputStream excelStream) throws IOException {
        this.attrMap = attrMap;
        this.dictValueToIdMap = dictValueToIdMap;
        // 复制 InputStream 到字节数组，避免后续被关闭
        this.excelBytes = copyToByteArray(excelStream);
        // 从隐藏 sheet 读取中文标题到字段 key 的映射
        readHeaderMappingFromHiddenSheet();
        // 读取第1行（Excel 中的列标题行）建立 headerIndexToKeyMap
        readTitleRowMapping();
    }

    /**
     * 将 InputStream 复制到字节数组
     */
    private byte[] copyToByteArray(InputStream inputStream) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        byte[] data = new byte[4096];
        int nRead;
        while ((nRead = inputStream.read(data, 0, data.length)) != -1) {
            buffer.write(data, 0, nRead);
        }
        buffer.flush();
        return buffer.toByteArray();
    }

    /**
     * 判断字符串是否为 UUID 格式
     */
    private boolean isUUID(String str) {
        if (str == null || str.length() != 36) {
            return false;
        }
        return str.matches("[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}");
    }

    /**
     * 从隐藏 sheet 读取中文标题到字段 key 的映射
     */
    @SuppressWarnings({"java:S3776", "java:S135"})
    private void readHeaderMappingFromHiddenSheet() {
        log.info("开始读取隐藏 sheet _field_mapping...");
        try (XSSFWorkbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            int sheetIndex = workbook.getSheetIndex("_field_mapping");
            if (sheetIndex >= 0) {
                workbook.setSheetVisibility(sheetIndex, SheetVisibility.VISIBLE);
            }

            Sheet hiddenSheet = workbook.getSheet("_field_mapping");
            if (hiddenSheet == null) {
                log.warn("未找到隐藏 sheet '_field_mapping'，使用备用映射方式");
                buildFallbackMapping();
                return;
            }

            // 用于存储字段 key -> 字典ID 的映射
            Map<String, String> fieldKeyToDictIdMap = new HashMap<>();

            // 用于记录字典数据区域的起始行
            Integer dictDataStartRow = null;

            // 直接从隐藏 sheet 读取字段映射（XSSFWorkbook 自动处理共享字符串）
            for (int i = 0; i <= hiddenSheet.getLastRowNum(); i++) {
                Row row = hiddenSheet.getRow(i);
                if (row == null) {
                    continue;
                }

                Cell chineseTitleCell = row.getCell(0);
                Cell fieldKeyCell = row.getCell(1);
                Cell dictIdCell = row.getCell(3);

                if (chineseTitleCell != null && fieldKeyCell != null) {
                    String chineseTitle = chineseTitleCell.getStringCellValue();
                    if (chineseTitle.endsWith("*")) {
                        chineseTitle = chineseTitle.substring(0, chineseTitle.length() - 1);
                    }
                    String fieldKey = fieldKeyCell.getStringCellValue();

                    // 跳过表头行和非映射数据
                    if ("中文标题".equals(chineseTitle) || chineseTitle.isEmpty()) {
                        continue;
                    }

                    // 记录字典数据区域的起始行
                    if ("字典ID".equals(chineseTitle)) {
                        dictDataStartRow = i + 1; // 字典数据从下一行开始
                        continue;
                    }

                    if (!fieldKey.isEmpty()) {
                        // 跳过 UUID 格式的标题（这些是字典数据区域的行）
                        if (isUUID(chineseTitle)) {
                            continue;
                        }

                        headerToKeyMap.put(chineseTitle, fieldKey);
                        keyToHeaderMap.put(fieldKey, chineseTitle); // 反向映射：字段key -> 中文标题

                        // 记录字典类型字段的字典ID
                        if (dictIdCell != null) {
                            String dictId = dictIdCell.getStringCellValue();
                            if (dictId != null && !dictId.isEmpty()) {
                                fieldKeyToDictIdMap.put(fieldKey, dictId);
                            }
                        }
                    }
                }
            }

            // 读取字典数据区域（如果存在）
            if (dictDataStartRow != null) {
                readDictDataFromHiddenSheet(hiddenSheet, dictDataStartRow, fieldKeyToDictIdMap);
            } else {
                log.warn("未找到字典数据区域");
            }

            // 补充操作类型的默认中文标题映射
            keyToHeaderMap.putIfAbsent("operationType", "操作类型");
        } catch (Exception e) {
            log.error("读取隐藏 sheet 失败，使用备用映射方式", e);
            buildFallbackMapping();
        }
    }

    /**
     * 从隐藏 sheet 读取字典数据区域，建立显示文本到数据ID的映射 字典数据区域格式：A列=字典ID，B列=数据ID，C列=显示文本
     */
    private void readDictDataFromHiddenSheet(Sheet hiddenSheet, int startRow, Map<String, String> fieldKeyToDictIdMap) {
        // 建立字典ID -> (显示文本 -> 数据ID) 的映射
        Map<String, Map<String, String>> dictIdToValueIdMap = new HashMap<>();

        for (int i = startRow; i <= hiddenSheet.getLastRowNum(); i++) {
            Row row = hiddenSheet.getRow(i);
            if (row == null) {
                continue;
            }

            Cell dictIdCell = row.getCell(0);
            Cell dataIdCell = row.getCell(1); // B列=数据ID（UUID）
            Cell displayTextCell = row.getCell(2); // C列=显示文本（用户看到的值）

            if (dictIdCell == null || dataIdCell == null || displayTextCell == null) {
                continue;
            }

            String dictId = dictIdCell.getStringCellValue();
            String dataId = dataIdCell.getStringCellValue(); // 数据ID（UUID）
            String displayText = displayTextCell.getStringCellValue(); // 显示文本

            if (dictId == null || dictId.isEmpty() || displayText == null || displayText.isEmpty() || dataId == null
                    || dataId.isEmpty()) {
                continue;
            }

            // 建立字典ID到显示文本/数据ID的映射
            dictIdToValueIdMap.computeIfAbsent(dictId, k -> new HashMap<>()).put(displayText, dataId);
        }

        // 将字典数据映射合并到 dictValueToIdMap（使用字段 key 作为 key）
        for (Map.Entry<String, String> entry : fieldKeyToDictIdMap.entrySet()) {
            String fieldKey = entry.getKey();
            String dictId = entry.getValue();

            Map<String, String> valueToIdMap = dictIdToValueIdMap.get(dictId);
            if (valueToIdMap != null && !valueToIdMap.isEmpty()) {
                dictValueToIdMap.put(fieldKey, valueToIdMap);
            } else {
                log.warn("字段 '{}' 使用的字典 '{}' 在隐藏 sheet 中未找到数据", fieldKey, dictId);
            }
        }
    }

    /**
     * 构建备用映射（直接从 attrMap 构建）
     */
    private void buildFallbackMapping() {
        for (Map.Entry<String, UserAttrResponseDto> entry : attrMap.entrySet()) {
            UserAttrResponseDto attr = entry.getValue();
            if (attr.getName() != null) {
                String fieldKey = entry.getKey();
                headerToKeyMap.put(attr.getName(), fieldKey);
                // 反向映射
                keyToHeaderMap.put(fieldKey, attr.getName());
            }
        }
        // 添加操作类型的中文标题映射
        headerToKeyMap.put("操作类型", "operationType");
        keyToHeaderMap.put("operationType", "操作类型");
    }

    /**
     * 读取第1行（Excel 中的列标题行）建立 headerIndexToKeyMap
     */
    @SuppressWarnings("java:S3776")
    private void readTitleRowMapping() {
        try (XSSFWorkbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            Sheet mainSheet = workbook.getSheetAt(0);
            Row titleRow = mainSheet.getRow(1);

            if (titleRow == null) {
                log.warn("未找到列标题行（第1行）");
                return;
            }

            for (int colIndex = 0; colIndex <= titleRow.getLastCellNum(); colIndex++) {
                Cell cell = titleRow.getCell(colIndex);
                if (cell == null) {
                    continue;
                }

                String chineseTitle = cell.getStringCellValue();
                if (chineseTitle == null || chineseTitle.isEmpty()) {
                    continue;
                }

                // 从 headerToKeyMap 中查找对应的字段 key
                String fieldKey = headerToKeyMap.get(chineseTitle);

                if (fieldKey != null) {
                    headerIndexToKeyMap.put(colIndex, fieldKey);
                    // 反向映射：字段key -> 中文标题
                    keyToHeaderMap.put(fieldKey, chineseTitle);
                } else {
                    // 尝试模糊匹配（不含 * 的情况）
                    for (Map.Entry<String, String> entry : headerToKeyMap.entrySet()) {
                        if (entry.getKey().replace("*", "").equals(chineseTitle.replace("*", ""))) {
                            headerIndexToKeyMap.put(colIndex, entry.getValue());
                            break;
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("读取列标题行失败", e);
        }
    }

    @Override
    public void invoke(Map<Integer, String> rowData, AnalysisContext context) {
        UserExcelImportDto dto = convertToDto(rowData);
        dataList.add(dto);
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
     * 将行数据转换为 DTO
     */
    @SuppressWarnings("java:S3776")
    private UserExcelImportDto convertToDto(Map<Integer, String> rowData) {
        UserExcelImportDto dto = new UserExcelImportDto();
        Map<String, Object> extAttrs = new HashMap<>();
        Map<String, Integer> extAttrColumnIndex = new HashMap<>();

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
                case "userId" :
                    dto.setUserId(value);
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
                        // 记录扩展字段的列索引（1-indexed，用于显示列号）
                        extAttrColumnIndex.put(fieldKey, colIndex + 1);
                    }
                    break;
            }
        }

        dto.setExtAttrs(extAttrs);
        dto.setExtAttrColumnIndex(extAttrColumnIndex);
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
            int month = Integer.parseInt(dateStr.substring(4, 6));
            int day = Integer.parseInt(dateStr.substring(6, 8));
            java.time.LocalDate date = java.time.LocalDate.of(year, month, day);
            return String.valueOf(date.atStartOfDay(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli());
        } catch (Exception e) {
            log.warn("日期转换失败: {}, 错误: {}", dateStr, e.getMessage());
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
            int month = Integer.parseInt(dateTimeStr.substring(4, 6));
            int day = Integer.parseInt(dateTimeStr.substring(6, 8));
            int hour = Integer.parseInt(dateTimeStr.substring(8, 10));
            int minute = Integer.parseInt(dateTimeStr.substring(10, 12));
            int second = Integer.parseInt(dateTimeStr.substring(12, 14));
            java.time.LocalDateTime dateTime = java.time.LocalDateTime.of(year, month, day, hour, minute, second);
            return String.valueOf(dateTime.atZone(java.time.ZoneId.systemDefault()).toInstant().toEpochMilli());
        } catch (Exception e) {
            log.warn("日期时间转换失败: {}, 错误: {}", dateTimeStr, e.getMessage());
            return dateTimeStr;
        }
    }
}
