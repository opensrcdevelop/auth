package cn.opensrcdevelop.auth.biz.util.excel;

import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.ClientAnchor;
import org.apache.poi.ss.usermodel.Comment;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Drawing;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFColor;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Component;

/**
 * Excel 模板生成器
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class ExcelTemplateGenerator {

    private final DictDataService dictDataService;

    // 颜色定义
    private static final byte[] HEADER_OPERATION_COLOR = new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80};
    private static final byte[] HEADER_BASIC_COLOR = new byte[]{(byte) 0x99, (byte) 0xCC, (byte) 0xFF};
    private static final byte[] HEADER_EXT_COLOR = new byte[]{(byte) 0xCC, (byte) 0xFF, (byte) 0xCC};

    // 必填字段（基础字段）
    private static final Set<String> REQUIRED_BASIC_FIELDS = Set.of("userId", "username");

    // 字段说明（key 为字段 key，用于基础字段的提示）
    private static final Map<String, String> FIELD_COMMENTS = Map.ofEntries(
            Map.entry("operationType", "0=新增，1=更新，2=删除"),
            Map.entry("userId", "更新/删除时必填，用于指定用户"),
            Map.entry("username", "必填，唯一标识"),
            Map.entry("emailAddress", "格式：example@domain.com"),
            Map.entry("phoneNumber", "格式：13800138000"),
            Map.entry("locked", "是/否"),
            Map.entry("consoleAccess", "是/否"),
            Map.entry("enableMfa", "是/否"));

    /**
     * 生成用户导入模版
     *
     * @param allFields
     *            所有用户字段（基础字段从代码定义，扩展字段从数据库）
     * @return Excel 文件字节数组
     */
    public byte[] generateTemplate(List<UserAttrResponseDto> allFields) {
        try (XSSFWorkbook workbook = new XSSFWorkbook()) {
            XSSFSheet sheet = workbook.createSheet("用户导入模版");

            // 创建隐藏 sheet（字段映射 + 字典数据）
            createHiddenSheet(workbook, allFields);

            // 计算列位置
            Map<String, Integer> columnPositions = calculateColumnPositions(allFields);

            // 创建表头（两行：父标题 + 子标题）
            int lastCol = createHeader(sheet, allFields, columnPositions);

            // 添加示例行
            createExampleRow(sheet, lastCol);

            // 设置数据验证（下拉列表）
            setDataValidations(sheet, workbook, allFields, columnPositions);

            // 调整列宽（按字符数计算）
            autoSizeColumns(sheet, lastCol, columnPositions, allFields);

            // 冻结前两行
            sheet.createFreezePane(0, 2);

            return workbookToByteArray(workbook);
        } catch (IOException e) {
            log.error("生成 Excel 模版失败", e);
            throw new RuntimeException("生成 Excel 模版失败", e);
        }
    }

    /**
     * 计算列位置 字段顺序：操作类型 -> 基础字段(userId, username 优先) -> 扩展字段
     */
    private Map<String, Integer> calculateColumnPositions(List<UserAttrResponseDto> allFields) {
        Map<String, Integer> positions = new LinkedHashMap<>();
        int colNum = 0;

        // 检查是否包含操作类型字段
        boolean hasOperationType = false;
        for (UserAttrResponseDto field : allFields) {
            if ("operationType".equals(field.getKey())) {
                hasOperationType = true;
                break;
            }
        }

        // 如果没有操作类型字段，手动添加
        if (!hasOperationType) {
            positions.put("operationType", colNum++);
        } else {
            positions.put("operationType", colNum++);
        }

        // 基础字段排序：userId 和 username 优先，其他按数据库顺序
        UserAttrResponseDto userIdField = null;
        UserAttrResponseDto usernameField = null;
        List<UserAttrResponseDto> otherBasicFields = new ArrayList<>();

        for (UserAttrResponseDto field : allFields) {
            String key = field.getKey();
            if ("operationType".equals(key)) {
                continue;
            }
            if (!Boolean.TRUE.equals(field.getExtFlg())) {
                if ("userId".equals(key)) {
                    userIdField = field;
                } else if ("username".equals(key)) {
                    usernameField = field;
                } else {
                    otherBasicFields.add(field);
                }
            }
        }

        // 添加 userId（位置 1，在操作类型之后）
        if (userIdField != null) {
            positions.put("userId", colNum++);
        }

        // 添加 username（位置 2，在 userId 之后）
        if (usernameField != null) {
            positions.put("username", colNum++);
        }

        // 添加其他基础字段
        for (UserAttrResponseDto field : otherBasicFields) {
            positions.put(field.getKey(), colNum++);
        }

        // 扩展字段（extFlg = true）
        for (UserAttrResponseDto field : allFields) {
            if (Boolean.TRUE.equals(field.getExtFlg())) {
                positions.put(field.getKey(), colNum++);
            }
        }

        return positions;
    }

    /**
     * 创建隐藏 sheet
     */
    private void createHiddenSheet(XSSFWorkbook workbook, List<UserAttrResponseDto> allFields) {
        XSSFSheet hiddenSheet = workbook.createSheet("_field_mapping");
        workbook.setSheetHidden(workbook.getSheetIndex(hiddenSheet), true);

        int rowNum = 0;

        // 写入字段映射信息（中文标题 -> 字段 key）
        Row mappingHeaderRow = hiddenSheet.createRow(rowNum++);
        mappingHeaderRow.createCell(0).setCellValue("中文标题");
        mappingHeaderRow.createCell(1).setCellValue("字段Key");
        mappingHeaderRow.createCell(2).setCellValue("数据类型");
        mappingHeaderRow.createCell(3).setCellValue("字典ID");

        // 操作类型
        Row operationRow = hiddenSheet.createRow(rowNum++);
        operationRow.createCell(0).setCellValue("操作类型*");
        operationRow.createCell(1).setCellValue("operationType");
        operationRow.createCell(2).setCellValue("NUMBER");

        // 基础字段
        for (UserAttrResponseDto field : allFields) {
            if (!Boolean.TRUE.equals(field.getExtFlg())) {
                Row row = hiddenSheet.createRow(rowNum++);
                // 使用与 attrMap.getName() 一致的标题（不带 * 号），便于导入时匹配
                row.createCell(0).setCellValue(field.getName());
                row.createCell(1).setCellValue(field.getKey());
                row.createCell(2).setCellValue(field.getDataType());
                if (field.getDictId() != null) {
                    row.createCell(3).setCellValue(field.getDictId());
                }
            }
        }

        // 扩展字段
        for (UserAttrResponseDto field : allFields) {
            if (Boolean.TRUE.equals(field.getExtFlg())) {
                Row row = hiddenSheet.createRow(rowNum++);
                row.createCell(0).setCellValue(field.getName());
                row.createCell(1).setCellValue(field.getKey());
                row.createCell(2).setCellValue(field.getDataType());
                if (field.getDictId() != null) {
                    row.createCell(3).setCellValue(field.getDictId());
                }
            }
        }

        // 写入字典数据（按显示名称排序）
        rowNum++; // 空行
        Row dictHeaderRow = hiddenSheet.createRow(rowNum++);
        dictHeaderRow.createCell(0).setCellValue("字典ID");
        dictHeaderRow.createCell(1).setCellValue("数据ID");
        dictHeaderRow.createCell(2).setCellValue("显示文本");

        Map<String, List<DictDataResponseDto>> dictDataMap = new HashMap<>();
        for (UserAttrResponseDto field : allFields) {
            if (field.getDictId() != null && !dictDataMap.containsKey(field.getDictId())) {
                List<DictDataResponseDto> dictData = dictDataService.getEnabledDictData(field.getDictId());
                dictDataMap.put(field.getDictId(), dictData);
            }
        }

        for (Map.Entry<String, List<DictDataResponseDto>> entry : dictDataMap.entrySet()) {
            // 使用 DictTreeFlattener 扁平化字典数据（包含子字典）
            List<DictTreeFlattener.DictDisplayItem> flattenedItems = DictTreeFlattener
                    .flattenTreeWithId(entry.getValue());
            // 按显示名称排序
            flattenedItems.sort(Comparator.comparing(DictTreeFlattener.DictDisplayItem::displayText));

            for (DictTreeFlattener.DictDisplayItem item : flattenedItems) {
                Row row = hiddenSheet.createRow(rowNum++);
                row.createCell(0).setCellValue(entry.getKey());
                row.createCell(1).setCellValue(item.id());
                row.createCell(2).setCellValue(item.displayText());
            }
        }
    }

    /**
     * 创建表头（两行：父标题 + 子标题）
     */
    private int createHeader(XSSFSheet sheet, List<UserAttrResponseDto> allFields,
            Map<String, Integer> columnPositions) {
        log.info("=== createHeader 开始 ===");
        log.info("columnPositions: {}", columnPositions);

        // 获取操作类型列位置
        int operationCol = columnPositions.get("operationType");
        log.info("operationCol: {}", operationCol);

        // 计算基础字段和扩展字段的列范围
        // 基础信息从操作类型列之后开始（排除操作类型列）
        int firstBasicCol = operationCol + 1;
        log.info("firstBasicCol: {}", firstBasicCol);

        // 基础字段数量（extFlg = false）
        int basicFieldCount = 0;
        for (UserAttrResponseDto field : allFields) {
            if (!Boolean.TRUE.equals(field.getExtFlg())) {
                basicFieldCount++;
            }
        }
        log.info("basicFieldCount: {}", basicFieldCount);

        int lastBasicCol = firstBasicCol + basicFieldCount - 1;
        log.info("lastBasicCol (基础信息最后列): {} (列字母: {})", lastBasicCol, (char) ('A' + lastBasicCol));

        // 扩展字段起始位置
        int firstExtCol = lastBasicCol + 1;
        log.info("firstExtCol (扩展信息起始列): {} (列字母: {})", firstExtCol, (char) ('A' + firstExtCol));

        // 从扩展字段起始位置开始计数
        int colNum = firstExtCol;
        int extFieldCount = 0;
        for (UserAttrResponseDto field : allFields) {
            if (field.getExtFlg()) {
                colNum++;
                extFieldCount++;
            }
        }
        int lastExtCol = colNum - 1;
        log.info("lastExtCol (扩展信息最后列): {} (列字母: {})", lastExtCol, (char) ('A' + lastExtCol));
        log.info("扩展字段数量: {}", extFieldCount);

        // 第一行：父标题
        Row parentRow = sheet.createRow(0);
        CellStyle parentStyle = createParentHeaderStyle(sheet.getWorkbook());

        // 操作类型父标题
        Cell operationParent = parentRow.createCell(operationCol);
        operationParent.setCellValue("操作信息");
        operationParent.setCellStyle(parentStyle);

        // 基础信息父标题（至少2列才合并）
        if (firstBasicCol < lastBasicCol) {
            Cell basicParent = parentRow.createCell(firstBasicCol);
            basicParent.setCellValue("基础信息");
            basicParent.setCellStyle(parentStyle);
            log.info("创建基础信息合并区域: CellRangeAddress(0, 0, {}, {}) -> {}-{}",
                    firstBasicCol, lastBasicCol, (char) ('A' + firstBasicCol), (char) ('A' + lastBasicCol));
            sheet.addMergedRegion(new CellRangeAddress(0, 0, firstBasicCol, lastBasicCol));
        } else if (firstBasicCol == lastBasicCol) {
            // 只有一个基础字段时，不合并，直接设置值
            Cell basicParent = parentRow.createCell(firstBasicCol);
            basicParent.setCellValue("基础信息");
            basicParent.setCellStyle(parentStyle);
        }

        // 扩展信息父标题（至少2列才合并）
        if (firstExtCol < lastExtCol) {
            Cell extParent = parentRow.createCell(firstExtCol);
            extParent.setCellValue("扩展信息");
            extParent.setCellStyle(parentStyle);
            log.info("创建扩展信息合并区域: CellRangeAddress(0, 0, {}, {}) -> {}-{}",
                    firstExtCol, lastExtCol, (char) ('A' + firstExtCol), (char) ('A' + lastExtCol));
            sheet.addMergedRegion(new CellRangeAddress(0, 0, firstExtCol, lastExtCol));
        } else if (firstExtCol == lastExtCol) {
            // 只有一个扩展字段时，不合并，直接设置值
            Cell extParent = parentRow.createCell(firstExtCol);
            extParent.setCellValue("扩展信息");
            extParent.setCellStyle(parentStyle);
        }

        // 第二行：子标题
        Row headerRow = sheet.createRow(1);

        // 操作类型列
        CellStyle operationStyle = createHeaderStyle(sheet.getWorkbook(), HEADER_OPERATION_COLOR);
        Cell operationCell = headerRow.createCell(operationCol);
        operationCell.setCellValue("操作类型*");
        operationCell.setCellStyle(operationStyle);
        addCellComment(sheet, operationCell, "operationType", 1, operationCol);

        // 基础字段（extFlg = false）
        CellStyle basicStyle = createHeaderStyle(sheet.getWorkbook(), HEADER_BASIC_COLOR);

        // 从 allFields 中获取基础字段
        for (UserAttrResponseDto field : allFields) {
            if (!Boolean.TRUE.equals(field.getExtFlg())) {
                String key = field.getKey();
                Integer colPos = columnPositions.get(key);
                if (colPos != null) {
                    boolean isRequired = REQUIRED_BASIC_FIELDS.contains(key);
                    Cell cell = headerRow.createCell(colPos);
                    // 使用中文名称作为列标题
                    cell.setCellValue(field.getName() + (isRequired ? "*" : ""));
                    cell.setCellStyle(basicStyle);
                    addCellComment(sheet, cell, key, 1, colPos);
                }
            }
        }

        // 扩展字段（extFlg = true）
        CellStyle extStyle = createHeaderStyle(sheet.getWorkbook(), HEADER_EXT_COLOR);
        for (UserAttrResponseDto field : allFields) {
            if (Boolean.TRUE.equals(field.getExtFlg())) {
                String key = field.getKey();
                Integer colPos = columnPositions.get(key);
                if (colPos != null) {
                    Cell cell = headerRow.createCell(colPos);
                    // 使用中文名称作为列标题
                    cell.setCellValue(field.getName());
                    cell.setCellStyle(extStyle);
                    // 为扩展字段添加说明
                    String comment = getExtensionFieldComment(field);
                    if (comment != null) {
                        addCellComment(sheet, cell, key, 1, colPos, comment);
                    }
                }
            }
        }

        return lastExtCol + 1; // 返回最后一列索引 + 1（用于循环）
    }

    /**
     * 获取扩展字段说明
     */
    private String getExtensionFieldComment(UserAttrResponseDto field) {
        StringBuilder comment = new StringBuilder();

        if (UserAttrDataTypeEnum.DICT.getType().equals(field.getDataType())) {
            comment.append("请从下拉列表中选择");
        } else if (UserAttrDataTypeEnum.BOOLEAN.getType().equals(field.getDataType())) {
            comment.append("是/否");
        } else if (UserAttrDataTypeEnum.DATE.getType().equals(field.getDataType())) {
            comment.append("格式：20260109");
        } else if (UserAttrDataTypeEnum.DATETIME.getType().equals(field.getDataType())) {
            comment.append("格式：20260109163045");
        } else if (UserAttrDataTypeEnum.NUMBER.getType().equals(field.getDataType())) {
            comment.append("请输入数字");
        }

        return comment.length() > 0 ? comment.toString() : null;
    }

    /**
     * 添加单元格注释（使用 fieldKey 从 FIELD_COMMENTS 获取说明）
     */
    private void addCellComment(XSSFSheet sheet, Cell cell, String fieldKey, int row, int col) {
        addCellComment(sheet, cell, fieldKey, row, col, FIELD_COMMENTS.get(fieldKey));
    }

    /**
     * 添加单元格注释（自定义内容）
     */
    private void addCellComment(XSSFSheet sheet, Cell cell, String author, int row, int col, String commentText) {
        if (commentText == null || commentText.isEmpty()) {
            return;
        }

        // 检查是否已有注释
        if (cell.getCellComment() != null) {
            return;
        }

        // 获取或创建 drawing patriarch
        Drawing<?> drawing = sheet.getDrawingPatriarch();
        if (drawing == null) {
            drawing = sheet.createDrawingPatriarch();
        }

        CreationHelper factory = sheet.getWorkbook().getCreationHelper();

        ClientAnchor anchor = factory.createClientAnchor();
        anchor.setCol1(col);
        anchor.setCol2(col + 3);
        anchor.setRow1(row);
        anchor.setRow2(row + 4);

        Comment comment = drawing.createCellComment(anchor);
        comment.setString(factory.createRichTextString(commentText));
        comment.setAuthor(author);
        cell.setCellComment(comment);
    }

    /**
     * 创建示例行
     */
    private void createExampleRow(XSSFSheet sheet, int lastCol) {
        Row exampleRow = sheet.createRow(2);

        // 创建左对齐的数据样式
        CellStyle dataStyle = sheet.getWorkbook().createCellStyle();
        XSSFFont dataFont = (XSSFFont) sheet.getWorkbook().createFont();
        dataFont.setFontName("Arial");
        dataFont.setFontHeightInPoints((short) 11);
        dataStyle.setFont(dataFont);
        dataStyle.setAlignment(HorizontalAlignment.LEFT);
        dataStyle.setVerticalAlignment(VerticalAlignment.CENTER);

        // 操作类型单元格
        Cell operationCell = exampleRow.createCell(0);
        operationCell.setCellValue("0");
        operationCell.setCellStyle(dataStyle);

        // 其他单元格
        for (int i = 1; i < lastCol; i++) {
            Cell cell = exampleRow.createCell(i);
            cell.setCellValue("");
            cell.setCellStyle(dataStyle);
        }
    }

    /**
     * 设置数据验证
     */
    private void setDataValidations(XSSFSheet sheet, XSSFWorkbook workbook, List<UserAttrResponseDto> allFields,
            Map<String, Integer> columnPositions) {
        DataValidationHelper validationHelper = sheet.getDataValidationHelper();

        // 操作类型下拉列表
        Integer operationCol = columnPositions.get("operationType");
        if (operationCol != null) {
            DataValidationConstraint operationConstraint = validationHelper.createExplicitListConstraint(
                    new String[]{"0", "1", "2"});
            CellRangeAddressList operationRange = new CellRangeAddressList(2, 10000, operationCol, operationCol);
            DataValidation operationValidation = validationHelper.createValidation(operationConstraint, operationRange);
            sheet.addValidationData(operationValidation);
        }

        // 为每个字段设置数据验证
        for (UserAttrResponseDto field : allFields) {
            String fieldKey = field.getKey();
            if ("operationType".equals(fieldKey)) {
                continue; // 跳过操作类型
            }
            Integer colNum = columnPositions.get(fieldKey);
            if (colNum == null) {
                continue;
            }

            if (UserAttrDataTypeEnum.DICT.getType().equals(field.getDataType()) && field.getDictId() != null) {
                // 字典类型：使用隐藏 sheet 中的数据
                setDictValidation(sheet, workbook, field.getDictId(), colNum);
            } else if (UserAttrDataTypeEnum.BOOLEAN.getType().equals(field.getDataType())) {
                // 布尔类型：是/否
                DataValidationConstraint booleanConstraint = validationHelper.createExplicitListConstraint(
                        new String[]{"是", "否"});
                CellRangeAddressList booleanRange = new CellRangeAddressList(2, 10000, colNum, colNum);
                DataValidation booleanValidation = validationHelper.createValidation(booleanConstraint, booleanRange);
                sheet.addValidationData(booleanValidation);
            }
        }
    }

    /**
     * 设置字典数据验证
     */
    private void setDictValidation(XSSFSheet sheet, XSSFWorkbook workbook, String dictId, int colNum) {
        Sheet hiddenSheet = workbook.getSheet("_field_mapping");
        if (hiddenSheet == null) {
            return;
        }

        // 收集该字典的所有数据行（支持不连续的行）
        List<Integer> dataRows = new ArrayList<>();

        for (int i = 0; i <= hiddenSheet.getLastRowNum(); i++) {
            Row row = hiddenSheet.getRow(i);
            if (row != null) {
                Cell cell = row.getCell(0);
                if (cell != null && dictId.equals(cell.getStringCellValue())) {
                    dataRows.add(i);
                }
            }
        }

        if (!dataRows.isEmpty()) {
            // 使用第一个和最后一个数据行构建范围
            int startRow = dataRows.get(0);
            int endRow = dataRows.get(dataRows.size() - 1);
            // 直接引用隐藏 sheet C 列的数据范围
            String formula = "_field_mapping!$C$" + (startRow + 1) + ":$C$" + (endRow + 1);
            DataValidationHelper validationHelper = sheet.getDataValidationHelper();
            DataValidationConstraint dictConstraint = validationHelper.createFormulaListConstraint(formula);
            CellRangeAddressList dictRange = new CellRangeAddressList(2, 10000, colNum, colNum);
            DataValidation dictValidation = validationHelper.createValidation(dictConstraint, dictRange);
            sheet.addValidationData(dictValidation);
        }
    }

    /**
     * 自动调整列宽 根据字段名称的字符数计算列宽：宽度 = (最大字符数 + 2) * 256
     */
    private void autoSizeColumns(XSSFSheet sheet, int lastCol, Map<String, Integer> columnPositions,
            List<UserAttrResponseDto> allFields) {
        for (int i = 0; i < lastCol; i++) {
            // 查找当前列对应的字段名称
            String fieldName = null;

            // 查找字段名称
            for (Map.Entry<String, Integer> entry : columnPositions.entrySet()) {
                if (entry.getValue() == i) {
                    String fieldKey = entry.getKey();
                    // 从 allFields 中查找
                    for (UserAttrResponseDto field : allFields) {
                        if (field.getKey().equals(fieldKey)) {
                            fieldName = field.getName();
                            break;
                        }
                    }
                    break;
                }
            }

            // 计算列宽：中文按 2 倍字符宽度计算
            int charWidth = fieldName != null ? calculateStringWidth(fieldName) : 10;
            int width = (charWidth + 2) * 256; // 留出左右边距
            width = Math.min(Math.max(width, 2000), 20000); // 限制在合理范围内
            sheet.setColumnWidth(i, width);
        }
    }

    /**
     * 计算字符串宽度（考虑中文字符）
     */
    private int calculateStringWidth(String text) {
        int width = 0;
        for (char c : text.toCharArray()) {
            if (c >= 0x4E00 && c <= 0x9FA5) {
                // 中文字符宽度为 2
                width += 2;
            } else if (c >= 'A' && c <= 'Z') {
                // 大写字母宽度为 1.2
                width += 1.2;
            } else if (c >= 'a' && c <= 'z') {
                // 小写字母宽度为 1
                width += 1;
            } else if (c >= '0' && c <= '9') {
                // 数字宽度为 0.8
                width += 0.8;
            } else {
                // 其他字符宽度为 1
                width += 1;
            }
        }
        return (int) Math.ceil(width);
    }

    /**
     * 创建父标题样式
     */
    private CellStyle createParentHeaderStyle(Workbook workbook) {
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

        style.setAlignment(org.apache.poi.ss.usermodel.HorizontalAlignment.CENTER);
        style.setVerticalAlignment(org.apache.poi.ss.usermodel.VerticalAlignment.CENTER);

        return style;
    }

    /**
     * 创建表头样式
     */
    private CellStyle createHeaderStyle(Workbook workbook, byte[] rgbColor) {
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

        style.setAlignment(org.apache.poi.ss.usermodel.HorizontalAlignment.CENTER);
        style.setVerticalAlignment(org.apache.poi.ss.usermodel.VerticalAlignment.CENTER);

        return style;
    }

    /**
     * 将工作簿转换为字节数组
     */
    private byte[] workbookToByteArray(XSSFWorkbook workbook) throws IOException {
        try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
            workbook.write(out);
            return out.toByteArray();
        }
    }
}
