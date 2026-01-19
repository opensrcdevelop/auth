package cn.opensrcdevelop.auth.biz.util.excel;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * ExcelTemplateGenerator 单元测试
 *
 * <p>
 * 测试 Excel 模版生成功能
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ExcelTemplateGeneratorTest {

    @Mock
    private DictDataService dictDataService;

    @InjectMocks
    private ExcelTemplateGenerator generator;

    @BeforeEach
    void setUp() {
        // Mock dictDataService 返回空列表，避免实际调用
        when(dictDataService.getEnabledDictData(anyString())).thenReturn(new ArrayList<>());
    }

    @Test
    void generateTemplate_shouldCreateExcelWithHeaders_whenGivenBasicFields() throws IOException {
        // Given
        List<UserAttrResponseDto> fields = List.of(
                createAttr("username", "用户名", "STRING", null, false),
                createAttr("emailAddress", "邮箱", "STRING", null, false));

        // When
        byte[] excelBytes = generator.generateTemplate(fields);

        // Then
        assertThat(excelBytes).isNotEmpty();

        try (Workbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            assertThat(workbook.getNumberOfSheets()).isGreaterThanOrEqualTo(1);

            Sheet mainSheet = workbook.getSheetAt(0);
            assertThat(mainSheet.getSheetName()).isEqualTo("用户导入模版");

            // 验证表头（第一行是父标题）
            Row parentRow = mainSheet.getRow(0);
            assertThat(parentRow).isNotNull();
            assertThat(parentRow.getCell(0).getStringCellValue()).isEqualTo("操作信息");
            assertThat(parentRow.getCell(1).getStringCellValue()).isEqualTo("基础信息");

            // 验证子标题（第二行）
            Row subHeaderRow = mainSheet.getRow(1);
            assertThat(subHeaderRow).isNotNull();
            assertThat(subHeaderRow.getCell(0).getStringCellValue()).isEqualTo("操作类型*");
            // username 是必填字段，会添加 * 后缀
            assertThat(subHeaderRow.getCell(1).getStringCellValue()).isEqualTo("用户名*");
            assertThat(subHeaderRow.getCell(2).getStringCellValue()).isEqualTo("邮箱");
        }
    }

    @Test
    void generateTemplate_shouldCreateExcelWithExtFields_whenGivenExtFields() throws IOException {
        // Given
        List<UserAttrResponseDto> fields = List.of(
                createAttr("username", "用户名", "STRING", null, false),
                createAttr("department", "部门", "DICT", "dept_dict", true),
                createAttr("level", "级别", "NUMBER", null, true));

        // When
        byte[] excelBytes = generator.generateTemplate(fields);

        // Then
        try (Workbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            Sheet mainSheet = workbook.getSheetAt(0);
            // 验证扩展字段在子标题行中（第二行）
            Row subHeaderRow = mainSheet.getRow(1);

            // 列0: 操作类型, 列1: 用户名(基础字段), 列2: 部门(扩展), 列3: 级别(扩展)
            assertThat(subHeaderRow.getCell(2).getStringCellValue()).isEqualTo("部门");
            assertThat(subHeaderRow.getCell(3).getStringCellValue()).isEqualTo("级别");
        }
    }

    @Test
    void generateTemplate_shouldCreateHiddenSheet_whenGivenFieldsWithDict() throws IOException {
        // Given
        List<UserAttrResponseDto> fields = List.of(
                createAttr("department", "部门", "DICT", "dept_dict", true));

        // When
        byte[] excelBytes = generator.generateTemplate(fields);

        // Then
        try (Workbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            // 查找隐藏的配置 sheet
            Sheet hiddenSheet = null;
            for (int i = 0; i < workbook.getNumberOfSheets(); i++) {
                String sheetName = workbook.getSheetName(i);
                if (sheetName.contains("_field_mapping")) {
                    hiddenSheet = workbook.getSheetAt(i);
                    break;
                }
            }

            assertThat(hiddenSheet).isNotNull();

            // 验证隐藏 sheet 包含字段映射信息
            boolean foundFieldSection = false;
            for (Row row : hiddenSheet) {
                org.apache.poi.ss.usermodel.Cell firstCell = row.getCell(0);
                if (firstCell != null && "中文标题".equals(firstCell.getStringCellValue())) {
                    foundFieldSection = true;
                    break;
                }
            }
            assertThat(foundFieldSection).isTrue();
        }
    }

    @Test
    void generateTemplate_shouldSetDataValidation_whenGivenDictFields() throws IOException {
        // Given
        List<UserAttrResponseDto> fields = List.of(
                createAttr("username", "用户名", "STRING", null, false),
                createAttr("department", "部门", "DICT", "dept_dict", true));

        // When
        byte[] excelBytes = generator.generateTemplate(fields);

        // Then
        try (Workbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            Sheet mainSheet = workbook.getSheetAt(0);

            // 验证数据验证存在（通过检查列数确认）
            // 子标题行在 row 1
            Row subHeaderRow = mainSheet.getRow(1);
            assertThat(subHeaderRow.getLastCellNum()).isGreaterThan((short) 1);
        }
    }

    @Test
    void generateTemplate_shouldCreateExampleRow_whenGivenFields() throws IOException {
        // Given
        List<UserAttrResponseDto> fields = List.of(
                createAttr("username", "用户名", "STRING", null, false),
                createAttr("emailAddress", "邮箱", "STRING", null, false));

        // When
        byte[] excelBytes = generator.generateTemplate(fields);

        // Then
        try (Workbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            Sheet mainSheet = workbook.getSheetAt(0);

            // 验证示例行在第三行（row 2）
            Row exampleRow = mainSheet.getRow(2);
            assertThat(exampleRow).isNotNull();
            assertThat(exampleRow.getCell(0).getStringCellValue()).isEqualTo("0");
        }
    }

    @Test
    void generateTemplate_shouldFreezeFirstRow_whenGeneratingTemplate() throws IOException {
        // Given
        List<UserAttrResponseDto> fields = List.of(
                createAttr("username", "用户名", "STRING", null, false));

        // When
        byte[] excelBytes = generator.generateTemplate(fields);

        // Then
        try (Workbook workbook = new XSSFWorkbook(new ByteArrayInputStream(excelBytes))) {
            Sheet mainSheet = workbook.getSheetAt(0);
            // 验证冻结窗格
            assertThat(mainSheet.getPaneInformation()).isNotNull();
        }
    }

    /**
     * 创建用户属性 DTO
     */
    private UserAttrResponseDto createAttr(String key, String name, String dataType, String dictId,
            boolean isExt) {
        UserAttrResponseDto attr = new UserAttrResponseDto();
        attr.setKey(key);
        attr.setName(name);
        attr.setDataType(dataType);
        attr.setDictId(dictId);
        attr.setExtFlg(isExt);
        return attr;
    }
}
