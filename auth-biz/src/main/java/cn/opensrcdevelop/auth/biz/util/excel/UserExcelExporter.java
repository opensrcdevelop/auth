package cn.opensrcdevelop.auth.biz.util.excel;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.springframework.stereotype.Component;

/**
 * 用户数据导出器
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class UserExcelExporter {

    private final DictDataService dictDataService;

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    // 颜色定义
    private static final byte[] HEADER_BASIC_COLOR = new byte[]{(byte) 0x99, (byte) 0xCC, (byte) 0xFF};
    private static final byte[] HEADER_EXT_COLOR = new byte[]{(byte) 0xCC, (byte) 0xFF, (byte) 0xCC};
    private static final byte[] CELL_BORDER_COLOR = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00};

    /**
     * 导出用户数据
     *
     * @param userList
     *            用户数据列表
     * @param allFields
     *            所有用户字段
     * @param filters
     *            筛选条件
     * @return Excel 文件字节数组
     */
    public byte[] exportUsers(List<Map<String, Object>> userList, List<UserAttrResponseDto> allFields,
            List<DataFilterDto> filters) {
        try (SXSSFWorkbook workbook = new SXSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("用户数据");

            // 分离基础字段和扩展字段
            List<UserAttrResponseDto> basicFields = new ArrayList<>();
            List<UserAttrResponseDto> extAttrs = new ArrayList<>();
            Map<String, UserAttrResponseDto> fieldMap = new HashMap<>();

            for (UserAttrResponseDto field : allFields) {
                if (field == null || field.getKey() == null) {
                    continue;
                }
                fieldMap.put(field.getKey(), field);
                if (Boolean.TRUE.equals(field.getExtFlg())) {
                    extAttrs.add(field);
                } else {
                    basicFields.add(field);
                }
            }

            // 按用户要求重新排序：userId 第一，username 第二
            reorderBasicFields(basicFields);

            // 2. 添加导出元数据
            int metaRowIdx = addExportMetadata(sheet, userList, filters, fieldMap, workbook);

            // 3. 创建表头（两行：父标题 + 子标题）
            int lastCol = createExportHeader(sheet, basicFields, extAttrs, metaRowIdx, workbook);

            // 4. 填充数据
            fillExportData(sheet, userList, basicFields, extAttrs, metaRowIdx, workbook);

            // 5. 冻结表头行
            sheet.createFreezePane(0, metaRowIdx + 2);

            // 6. 自动调整列宽（考虑父标题行的合并）
            int dataStartRow = metaRowIdx + 2;
            autoSizeColumns(sheet, lastCol, userList, basicFields, extAttrs, dataStartRow);

            // 7. 写入输出流
            try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                workbook.write(out);
                return out.toByteArray();
            }
        } catch (IOException e) {
            log.error("导出用户数据失败", e);
            throw new RuntimeException("导出用户数据失败", e);
        }
    }

    /**
     * 重新排序基础字段：userId 第一，username 第二
     */
    private void reorderBasicFields(List<UserAttrResponseDto> basicFields) {
        UserAttrResponseDto userIdField = null;
        UserAttrResponseDto usernameField = null;
        UserAttrResponseDto createTimeField = null;
        List<UserAttrResponseDto> others = new ArrayList<>();

        for (UserAttrResponseDto field : basicFields) {
            if (field == null || field.getKey() == null) {
                continue;
            }
            String key = field.getKey();
            if ("userId".equals(key)) {
                userIdField = field;
            } else if ("username".equals(key)) {
                usernameField = field;
            } else if ("createTime".equals(key)) {
                createTimeField = field;
            } else {
                others.add(field);
            }
        }

        basicFields.clear();
        if (userIdField != null) {
            basicFields.add(userIdField);
        }
        if (usernameField != null) {
            basicFields.add(usernameField);
        }
        // 将创建时间放在用户名后面
        if (createTimeField != null) {
            basicFields.add(createTimeField);
        }
        basicFields.addAll(others);
    }

    /**
     * 添加导出元数据（前几行）
     */
    private int addExportMetadata(Sheet sheet, List<Map<String, Object>> userList, List<DataFilterDto> filters,
            Map<String, UserAttrResponseDto> fieldMap, SXSSFWorkbook workbook) {
        int rowIdx = 0;

        // 第 1 行：导出时间
        Row timeRow = sheet.createRow(rowIdx++);
        CellStyle metaStyle = createMetaStyle(workbook);
        timeRow.createCell(0).setCellValue("导出时间");
        timeRow.createCell(1).setCellValue(LocalDateTime.now().format(DATETIME_FORMATTER));
        applyCellStyle(timeRow.getCell(0), metaStyle);
        applyCellStyle(timeRow.getCell(1), metaStyle);

        // 第 2 行：用户数
        Row countRow = sheet.createRow(rowIdx++);
        countRow.createCell(0).setCellValue("用户数");
        countRow.createCell(1).setCellValue(userList.size());
        applyCellStyle(countRow.getCell(0), metaStyle);
        applyCellStyle(countRow.getCell(1), metaStyle);

        // 第 3 行起：筛选条件（表格形式）
        if (filters != null && !filters.isEmpty()) {
            Row filterHeaderRow = sheet.createRow(rowIdx++);
            filterHeaderRow.createCell(0).setCellValue("筛选条件");
            applyCellStyle(filterHeaderRow.getCell(0), metaStyle);

            CellStyle filterValueStyle = createFilterValueStyle(workbook);
            for (DataFilterDto filter : filters) {
                UserAttrResponseDto field = fieldMap.get(filter.getKey());
                String fieldName = field != null && field.getName() != null ? field.getName() : filter.getKey();

                Row filterRow = sheet.createRow(rowIdx++);
                filterRow.createCell(0).setCellValue(fieldName);
                filterRow.createCell(1).setCellValue(getFilterOperatorText(filter.getFilterType()));
                filterRow.createCell(2).setCellValue(formatFilterValue(filter, field));
                applyCellStyle(filterRow.getCell(0), filterValueStyle);
                applyCellStyle(filterRow.getCell(1), filterValueStyle);
                applyCellStyle(filterRow.getCell(2), filterValueStyle);
            }
        } else {
            Row noFilterRow = sheet.createRow(rowIdx++);
            noFilterRow.createCell(0).setCellValue("筛选条件");
            noFilterRow.createCell(1).setCellValue("无");
            applyCellStyle(noFilterRow.getCell(0), metaStyle);
            applyCellStyle(noFilterRow.getCell(1), metaStyle);
        }

        // 空行分隔
        sheet.createRow(rowIdx++);

        return rowIdx;
    }

    /**
     * 获取筛选操作符的中文描述
     */
    private String getFilterOperatorText(String filterType) {
        if (filterType == null) {
            return "";
        }
        return switch (filterType) {
            case "EQ" -> "等于";
            case "NE" -> "不等于";
            case "LIKE" -> "包含";
            case "GT" -> "大于";
            case "GTE" -> "大于等于";
            case "LT" -> "小于";
            case "LTE" -> "小于等于";
            case "IN" -> "在列表中";
            case "NOT_IN" -> "不在列表中";
            case "IS_NULL" -> "为空";
            case "IS_NOT_NULL" -> "不为空";
            default -> filterType;
        };
    }

    /**
     * 格式化筛选条件的值
     */
    private String formatFilterValue(DataFilterDto filter, UserAttrResponseDto field) {
        Object value = filter.getValue();
        if (value == null) {
            return "";
        }

        String valueStr = String.valueOf(value);

        // 如果是字典类型，需要转换显示文本
        if (field != null && "DICT".equals(field.getDataType()) && field.getDictId() != null) {
            String displayText = formatDictValue(field.getDictId(), valueStr);
            if (displayText != null && !displayText.equals(valueStr)) {
                return displayText;
            }
        }

        // 如果是日期类型，需要格式化显示
        if (field != null) {
            String dataType = field.getDataType();
            if ("DATE".equals(dataType)) {
                return formatDateValue(value);
            } else if ("DATETIME".equals(dataType)) {
                return formatDateTimeValue(value);
            }
        }

        return valueStr;
    }

    /**
     * 创建导出表头
     */
    private int createExportHeader(Sheet sheet, List<UserAttrResponseDto> basicFields,
            List<UserAttrResponseDto> extAttrs, int startRow, SXSSFWorkbook workbook) {
        // 计算列范围
        int basicFieldCount = basicFields.size();
        int extFieldCount = extAttrs.size();
        int totalCols = basicFieldCount + extFieldCount;

        // 第一行：父标题（如果有扩展字段，则分两列显示基础信息和扩展信息）
        Row parentRow = sheet.createRow(startRow);
        CellStyle parentStyle = createParentHeaderStyle(workbook);

        if (basicFieldCount > 0 && extFieldCount > 0) {
            // 基础信息父标题（从第0列开始，跨越所有基础字段）
            Cell basicParent = parentRow.createCell(0);
            basicParent.setCellValue("基础信息");
            basicParent.setCellStyle(parentStyle);
            // 如果基础字段大于1列，合并区域
            if (basicFieldCount > 1) {
                sheet.addMergedRegion(
                        new CellRangeAddress(startRow, startRow, 0, basicFieldCount - 1));
            }
            // 填充基础字段跨越的单元格
            for (int i = 1; i < basicFieldCount; i++) {
                Cell cell = parentRow.createCell(i);
                cell.setCellStyle(parentStyle);
            }

            // 扩展信息父标题（从基础字段后开始）
            Cell extParent = parentRow.createCell(basicFieldCount);
            extParent.setCellValue("扩展信息");
            extParent.setCellStyle(parentStyle);
            // 如果扩展字段大于1列，合并区域
            if (extFieldCount > 1) {
                sheet.addMergedRegion(
                        new CellRangeAddress(startRow, startRow, basicFieldCount,
                                totalCols - 1));
            }
            // 填充扩展字段跨越的单元格
            for (int i = basicFieldCount + 1; i < totalCols; i++) {
                Cell cell = parentRow.createCell(i);
                cell.setCellStyle(parentStyle);
            }
        } else if (basicFieldCount > 0) {
            // 只有基础信息
            Cell basicParent = parentRow.createCell(0);
            basicParent.setCellValue("基础信息");
            basicParent.setCellStyle(parentStyle);
            if (totalCols > 1) {
                sheet.addMergedRegion(
                        new CellRangeAddress(startRow, startRow, 0, totalCols - 1));
            }
            // 填充所有单元格
            for (int i = 1; i < totalCols; i++) {
                Cell cell = parentRow.createCell(i);
                cell.setCellStyle(parentStyle);
            }
        } else if (extFieldCount > 0) {
            // 只有扩展信息
            Cell extParent = parentRow.createCell(0);
            extParent.setCellValue("扩展信息");
            extParent.setCellStyle(parentStyle);
            if (totalCols > 1) {
                sheet.addMergedRegion(
                        new CellRangeAddress(startRow, startRow, 0, totalCols - 1));
            }
            // 填充所有单元格
            for (int i = 1; i < totalCols; i++) {
                Cell cell = parentRow.createCell(i);
                cell.setCellStyle(parentStyle);
            }
        }

        // 第二行：子标题
        Row headerRow = sheet.createRow(startRow + 1);
        CellStyle basicStyle = createHeaderStyle(workbook, HEADER_BASIC_COLOR);
        CellStyle extStyle = createHeaderStyle(workbook, HEADER_EXT_COLOR);

        int colIdx = 0;
        // 基础字段
        for (UserAttrResponseDto field : basicFields) {
            Cell cell = headerRow.createCell(colIdx);
            String fieldName = field.getName();
            cell.setCellValue(fieldName != null ? fieldName : "");
            cell.setCellStyle(basicStyle);
            colIdx++;
        }
        // 扩展字段
        for (UserAttrResponseDto field : extAttrs) {
            Cell cell = headerRow.createCell(colIdx);
            String fieldName = field.getName();
            cell.setCellValue(fieldName != null ? fieldName : "");
            cell.setCellStyle(extStyle);
            colIdx++;
        }

        return colIdx;
    }

    /**
     * 填充导出数据
     */
    private void fillExportData(Sheet sheet, List<Map<String, Object>> userList, List<UserAttrResponseDto> basicFields,
            List<UserAttrResponseDto> extAttrs, int startRow, SXSSFWorkbook workbook) {
        CellStyle dataStyle = createDataStyle(workbook);

        int rowIdx = startRow + 2;
        for (Map<String, Object> userMap : userList) {
            Row dataRow = sheet.createRow(rowIdx++);

            int colIdx = 0;
            // 基础字段
            for (UserAttrResponseDto field : basicFields) {
                String value = getFieldValue(userMap, field.getKey(), field.getDataType());
                Cell cell = dataRow.createCell(colIdx);
                cell.setCellValue(value);
                applyCellStyle(cell, dataStyle);
                colIdx++;
            }
            // 扩展字段
            for (UserAttrResponseDto field : extAttrs) {
                Object value = userMap.get(field.getKey());
                String cellValue = formatExportValue(value, field);
                Cell cell = dataRow.createCell(colIdx);
                cell.setCellValue(cellValue);
                applyCellStyle(cell, dataStyle);
                colIdx++;
            }
        }
    }

    /**
     * 自动调整列宽（按当前列中的所有内容的最大字符数调整）
     */
    private void autoSizeColumns(Sheet sheet, int lastCol, List<Map<String, Object>> userList,
            List<UserAttrResponseDto> basicFields, List<UserAttrResponseDto> extAttrs, int dataStartRow) {
        for (int colIdx = 0; colIdx < lastCol; colIdx++) {
            int maxWidth = 0;

            // 考虑子标题（字段名称）
            Row headerRow = sheet.getRow(dataStartRow - 1);
            if (headerRow != null && headerRow.getCell(colIdx) != null) {
                String headerText = headerRow.getCell(colIdx).getStringCellValue();
                maxWidth = Math.max(maxWidth, calculateStringWidth(headerText));
            }

            // 考虑数据内容 - 只考虑当前列
            for (Map<String, Object> userMap : userList) {
                String value = "";
                if (colIdx < basicFields.size()) {
                    UserAttrResponseDto field = basicFields.get(colIdx);
                    value = getFieldValue(userMap, field.getKey(), field.getDataType());
                } else {
                    UserAttrResponseDto field = extAttrs.get(colIdx - basicFields.size());
                    value = formatExportValue(userMap.get(field.getKey()), field);
                }
                maxWidth = Math.max(maxWidth, calculateStringWidth(value));
            }

            // 如果没有数据，使用标题字符数
            if (maxWidth == 0 && headerRow != null && headerRow.getCell(colIdx) != null) {
                String headerText = headerRow.getCell(colIdx).getStringCellValue();
                maxWidth = calculateStringWidth(headerText);
            }

            // 最小宽度为 8 个字符
            maxWidth = Math.max(maxWidth, 8);

            int width = (maxWidth + 2) * 256;
            width = Math.min(Math.max(width, 2000), 20000);
            sheet.setColumnWidth(colIdx, width);
        }
    }

    /**
     * 计算字符串宽度（考虑中文字符）
     */
    private int calculateStringWidth(String text) {
        if (text == null || text.isEmpty()) {
            return 0;
        }
        int width = 0;
        for (char c : text.toCharArray()) {
            if (c >= 0x4E00 && c <= 0x9FA5) {
                width += 2;
            } else if (c >= 'A' && c <= 'Z') {
                width += 1.2;
            } else if (c >= 'a' && c <= 'z') {
                width += 1;
            } else if (c >= '0' && c <= '9') {
                width += 0.8;
            } else {
                width += 1;
            }
        }
        return (int) Math.ceil(width);
    }

    /**
     * 根据数据类型获取字段值
     */
    private String getFieldValue(Map<String, Object> map, String key, String dataType) {
        if (map == null || key == null) {
            return "";
        }
        Object value = map.get(key);
        if (value == null) {
            return "";
        }

        String strValue = String.valueOf(value);

        if ("BOOLEAN".equals(dataType)) {
            return "true".equalsIgnoreCase(strValue) ? "是" : "否";
        } else if ("DATE".equals(dataType)) {
            return formatDateValue(value);
        } else if ("DATETIME".equals(dataType)) {
            return formatDateTimeValue(value);
        }

        return strValue;
    }

    /**
     * 格式化导出值
     */
    private String formatExportValue(Object value, UserAttrResponseDto attr) {
        if (value == null) {
            return "";
        }

        String dataType = attr.getDataType();
        if ("DICT".equals(dataType) && attr.getDictId() != null) {
            return formatDictValue(attr.getDictId(), String.valueOf(value));
        } else if ("BOOLEAN".equals(dataType)) {
            return "true".equalsIgnoreCase(String.valueOf(value)) ? "是" : "否";
        } else if ("DATE".equals(dataType)) {
            return formatDateValue(value);
        } else if ("DATETIME".equals(dataType)) {
            return formatDateTimeValue(value);
        }

        return String.valueOf(value);
    }

    /**
     * 格式化日期值
     */
    private String formatDateValue(Object value) {
        if (value == null) {
            return "";
        }
        String strValue = String.valueOf(value);
        // 如果是 ISO 格式，提取日期部分
        if (strValue.contains("T") || strValue.contains("-")) {
            try {
                if (strValue.length() >= 10) {
                    return strValue.substring(0, 10);
                }
            } catch (Exception e) {
                // 忽略格式化错误
            }
        }
        return strValue;
    }

    /**
     * 格式化日期时间值
     */
    private String formatDateTimeValue(Object value) {
        if (value == null) {
            return "";
        }
        String strValue = String.valueOf(value);
        // 如果是 ISO 格式，提取日期时间部分
        if (strValue.contains("T")) {
            try {
                // 2026-01-15T10:30:00 -> 2026-01-15 10:30:00
                strValue = strValue.replace("T", " ");
                if (strValue.contains(".")) {
                    strValue = strValue.substring(0, strValue.indexOf("."));
                }
                return strValue;
            } catch (Exception e) {
                // 忽略格式化错误
            }
        }
        return strValue;
    }

    /**
     * 格式化字典值为显示文本（使用扁平化后的路径）
     */
    private String formatDictValue(String dictId, String dataId) {
        try {
            List<DictDataResponseDto> dictData = dictDataService.getEnabledDictData(dictId);
            // 使用 DictTreeFlattener 构建完整路径
            List<DictTreeFlattener.DictDisplayItem> items = DictTreeFlattener.flattenTreeWithId(dictData);

            // 查找匹配的显示文本（精确匹配或末尾匹配）
            for (DictTreeFlattener.DictDisplayItem item : items) {
                if (item.id().equals(dataId) || item.displayText().endsWith(dataId)) {
                    return item.displayText();
                }
            }

            // 如果扁平化列表中没找到，直接在树中查找并构建路径
            String displayText = findDisplayTextWithPath(dictData, dataId, "");
            if (displayText != null && !displayText.isEmpty()) {
                return displayText;
            }
        } catch (Exception e) {
            log.warn("获取字典数据失败: dictId={}, dataId={}", dictId, dataId);
        }
        return dataId;
    }

    /**
     * 在字典树中查找数据ID并构建完整路径
     */
    private String findDisplayTextWithPath(List<DictDataResponseDto> dictData, String targetId, String currentPath) {
        for (DictDataResponseDto node : dictData) {
            // 构建当前节点的路径
            String nodePath = currentPath.isEmpty() ? node.getLabel() : currentPath + " / " + node.getLabel();

            // 如果是目标节点且没有子节点，返回完整路径
            if (node.getId().equals(targetId)) {
                if (node.getChildren() == null || node.getChildren().isEmpty()) {
                    return nodePath;
                }
                return null; // 有子节点不显示
            }

            // 递归查找子节点
            if (node.getChildren() != null) {
                String result = findDisplayTextWithPath(node.getChildren(), targetId, nodePath);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;
    }

    // ========== 样式创建方法 ==========

    private CellStyle createMetaStyle(SXSSFWorkbook workbook) {
        XSSFCellStyle style = (XSSFCellStyle) workbook.createCellStyle();
        XSSFFont font = (XSSFFont) workbook.createFont();
        font.setFontName("Arial");
        font.setFontHeightInPoints((short) 11);
        style.setFont(font);
        style.setAlignment(HorizontalAlignment.LEFT);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        return style;
    }

    private CellStyle createFilterValueStyle(SXSSFWorkbook workbook) {
        XSSFCellStyle style = (XSSFCellStyle) workbook.createCellStyle();
        XSSFFont font = (XSSFFont) workbook.createFont();
        font.setFontName("Arial");
        font.setFontHeightInPoints((short) 11);
        style.setFont(font);
        style.setAlignment(HorizontalAlignment.LEFT);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        style.setTopBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setBottomBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setLeftBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setRightBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        return style;
    }

    private CellStyle createParentHeaderStyle(SXSSFWorkbook workbook) {
        XSSFCellStyle style = (XSSFCellStyle) workbook.createCellStyle();
        XSSFColor color = new XSSFColor(new byte[]{(byte) 0xE0, (byte) 0xE0, (byte) 0xE0}, null);
        style.setFillForegroundColor(color);
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        XSSFFont font = (XSSFFont) workbook.createFont();
        font.setFontName("Arial");
        font.setFontHeightInPoints((short) 12);
        font.setBold(true);
        style.setFont(font);
        style.setBorderTop(BorderStyle.MEDIUM);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        style.setTopBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setBottomBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setLeftBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setRightBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setAlignment(HorizontalAlignment.CENTER);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        return style;
    }

    private CellStyle createHeaderStyle(SXSSFWorkbook workbook, byte[] rgbColor) {
        XSSFCellStyle style = (XSSFCellStyle) workbook.createCellStyle();
        XSSFColor color = new XSSFColor(rgbColor, null);
        style.setFillForegroundColor(color);
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        XSSFFont font = (XSSFFont) workbook.createFont();
        font.setFontName("Arial");
        font.setFontHeightInPoints((short) 11);
        font.setBold(true);
        style.setFont(font);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        style.setTopBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setBottomBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setLeftBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setRightBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setAlignment(HorizontalAlignment.CENTER);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        return style;
    }

    private CellStyle createDataStyle(SXSSFWorkbook workbook) {
        XSSFCellStyle style = (XSSFCellStyle) workbook.createCellStyle();
        XSSFFont font = (XSSFFont) workbook.createFont();
        font.setFontName("Arial");
        font.setFontHeightInPoints((short) 11);
        style.setFont(font);
        style.setAlignment(HorizontalAlignment.LEFT);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        style.setTopBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setBottomBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setLeftBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        style.setRightBorderColor(new XSSFColor(CELL_BORDER_COLOR, null));
        return style;
    }

    private void applyCellStyle(Cell cell, CellStyle style) {
        if (cell != null && style != null) {
            cell.setCellStyle(style);
        }
    }
}
