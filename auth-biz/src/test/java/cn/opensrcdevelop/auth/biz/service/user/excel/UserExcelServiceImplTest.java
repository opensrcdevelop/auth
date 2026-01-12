package cn.opensrcdevelop.auth.biz.service.user.excel;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.excel.impl.UserExcelServiceImpl;
import cn.opensrcdevelop.auth.biz.util.excel.ExcelTemplateGenerator;
import cn.opensrcdevelop.common.response.PageData;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;

/**
 * UserExcelServiceImpl 单元测试
 *
 * <p>
 * 测试用户导入导出服务功能
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class UserExcelServiceImplTest {

    @Mock
    private UserService userService;

    @Mock
    private UserAttrService userAttrService;

    @Mock
    private DictDataService dictDataService;

    @Mock
    private ExcelTemplateGenerator templateGenerator;

    private UserExcelServiceImpl userExcelService;

    @BeforeEach
    void setUp() {
        userExcelService = new UserExcelServiceImpl(userService, userAttrService, dictDataService,
                templateGenerator);

        // 默认 mock getAllUserAttrsForExcel 返回空列表
        when(userAttrService.getAllUserAttrsForExcel()).thenReturn(new ArrayList<>());

        // 默认 mock listUserAttrs 返回空分页数据（用于 importUsers）
        PageData<UserAttrResponseDto> emptyAttrs = new PageData<>();
        emptyAttrs.setList(new ArrayList<>());
        when(userAttrService.listUserAttrs(anyInt(), anyInt(), anyBoolean(), any())).thenReturn(emptyAttrs);

        // 默认 mock dictDataService.getEnabledDictData 返回空列表
        when(dictDataService.getEnabledDictData(anyString())).thenReturn(new ArrayList<>());
    }

    @Test
    void generateImportTemplate_shouldReturnValidExcel_whenCalled() {
        // Given
        byte[] mockExcelBytes = createMockExcelBytes();
        when(templateGenerator.generateTemplate(any())).thenReturn(mockExcelBytes);

        // Mock getAllUserAttrsForExcel 返回用户属性列表
        List<UserAttrResponseDto> mockAttrs = List.of(
                createMockAttr("username", "用户名", "STRING", null),
                createMockAttr("emailAddress", "邮箱", "STRING", null),
                createMockAttr("department", "部门", "DICT", "dept_dict"));
        when(userAttrService.getAllUserAttrsForExcel()).thenReturn(mockAttrs);

        // When
        byte[] result = userExcelService.generateImportTemplate();

        // Then
        assertThat(result).isNotEmpty();
        verify(userAttrService).getAllUserAttrsForExcel();
        verify(templateGenerator).generateTemplate(any());
    }

    @Test
    void exportUsers_shouldReturnValidExcel_whenGivenUsers() {
        // Given
        Map<String, Object> userMap = new HashMap<>();
        userMap.put("username", "testuser");
        userMap.put("emailAddress", "test@example.com");
        userMap.put("phoneNumber", "13800138000");
        userMap.put("locked", false);
        userMap.put("consoleAccess", true);
        userMap.put("enableMfa", false);

        PageData<Map<String, Object>> mockPageData = new PageData<>();
        mockPageData.setList(List.of(userMap));
        when(userService.list(anyInt(), anyInt(), any())).thenReturn(mockPageData);

        // When
        byte[] result = userExcelService.exportUsers(List.of(), false);

        // Then
        assertThat(result).isNotEmpty();
        verify(userService).list(1, 100, List.of());
    }

    @Test
    void exportUsers_shouldRequestMaxSize_whenExportAllIsTrue() {
        // Given
        PageData<Map<String, Object>> mockPageData = new PageData<>();
        mockPageData.setList(List.of());
        when(userService.list(anyInt(), anyInt(), any())).thenReturn(mockPageData);

        // When
        byte[] result = userExcelService.exportUsers(List.of(), true);

        // Then
        assertThat(result).isNotEmpty();
        verify(userService).list(1, Integer.MAX_VALUE, List.of());
    }

    @Test
    void exportUsers_shouldRequestDefaultSize_whenExportAllIsFalse() {
        // Given
        PageData<Map<String, Object>> mockPageData = new PageData<>();
        mockPageData.setList(List.of());
        when(userService.list(anyInt(), anyInt(), any())).thenReturn(mockPageData);

        // When
        byte[] result = userExcelService.exportUsers(List.of(), false);

        // Then
        assertThat(result).isNotEmpty();
        verify(userService).list(1, 100, List.of());
    }

    @Test
    void importUsers_shouldReturnSuccessResult_whenGivenValidExcel() throws Exception {
        // Given
        byte[] excelBytes = createValidImportExcel();
        MockMultipartFile file = new MockMultipartFile("file", "users.xlsx",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", excelBytes);

        // When
        ExcelImportResultDto result = userExcelService.importUsers(file);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getSuccessCount()).isGreaterThanOrEqualTo(0);
        assertThat(result.getFailureCount()).isGreaterThanOrEqualTo(0);
    }

    @Test
    void importUsers_shouldReturnErrors_whenGivenInvalidOperationType() throws Exception {
        // Given - 创建一个包含无效操作类型的 Excel
        byte[] excelBytes = createInvalidOperationTypeExcel();
        MockMultipartFile file = new MockMultipartFile("file", "users.xlsx",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", excelBytes);

        // When
        ExcelImportResultDto result = userExcelService.importUsers(file);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getFailureCount()).isGreaterThan(0);
        assertThat(result.getErrors()).isNotEmpty();
    }

    @Test
    void importUsers_shouldReturnErrors_whenMissingRequiredFields() throws Exception {
        // Given - 创建一个缺少必填字段的 Excel
        byte[] excelBytes = createMissingRequiredFieldsExcel();
        MockMultipartFile file = new MockMultipartFile("file", "users.xlsx",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", excelBytes);

        // When
        ExcelImportResultDto result = userExcelService.importUsers(file);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getFailureCount()).isGreaterThan(0);
    }

    /**
     * 创建有效的导入 Excel 文件
     */
    private byte[] createValidImportExcel() throws Exception {
        try (Workbook workbook = new SXSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("用户导入模版");

            // 表头
            Row headerRow = sheet.createRow(0);
            headerRow.createCell(0).setCellValue("操作类型");
            headerRow.createCell(1).setCellValue("用户名");
            headerRow.createCell(2).setCellValue("邮箱");

            // 数据行
            Row dataRow = sheet.createRow(1);
            dataRow.createCell(0).setCellValue(0); // 添加操作
            dataRow.createCell(1).setCellValue("testuser");
            dataRow.createCell(2).setCellValue("test@example.com");

            try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                workbook.write(out);
                return out.toByteArray();
            }
        }
    }

    /**
     * 创建包含无效操作类型的 Excel 文件
     */
    private byte[] createInvalidOperationTypeExcel() throws Exception {
        try (Workbook workbook = new SXSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("用户导入模版");

            // 表头
            Row headerRow = sheet.createRow(0);
            headerRow.createCell(0).setCellValue("操作类型");
            headerRow.createCell(1).setCellValue("用户名");
            headerRow.createCell(2).setCellValue("邮箱");

            // 数据行 - 无效的操作类型
            Row dataRow = sheet.createRow(1);
            dataRow.createCell(0).setCellValue(99); // 无效的操作类型
            dataRow.createCell(1).setCellValue("testuser");
            dataRow.createCell(2).setCellValue("test@example.com");

            try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                workbook.write(out);
                return out.toByteArray();
            }
        }
    }

    /**
     * 创建缺少必填字段的 Excel 文件
     */
    private byte[] createMissingRequiredFieldsExcel() throws Exception {
        try (Workbook workbook = new SXSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("用户导入模版");

            // 表头
            Row headerRow = sheet.createRow(0);
            headerRow.createCell(0).setCellValue("操作类型");
            headerRow.createCell(1).setCellValue("用户名");
            headerRow.createCell(2).setCellValue("邮箱");

            // 数据行 - 缺少用户名
            Row dataRow = sheet.createRow(1);
            dataRow.createCell(0).setCellValue(0); // 添加操作
            dataRow.createCell(1).setCellValue(""); // 空用户名
            dataRow.createCell(2).setCellValue("test@example.com");

            try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                workbook.write(out);
                return out.toByteArray();
            }
        }
    }

    /**
     * 创建模拟的 Excel 字节数组
     */
    private byte[] createMockExcelBytes() {
        try (Workbook workbook = new SXSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("测试");
            Row row = sheet.createRow(0);
            row.createCell(0).setCellValue("测试");
            try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                workbook.write(out);
                return out.toByteArray();
            }
        } catch (Exception e) {
            return new byte[0];
        }
    }

    /**
     * 创建模拟的用户属性
     */
    private UserAttrResponseDto createMockAttr(String key, String name, String dataType, String dictId) {
        UserAttrResponseDto attr = new UserAttrResponseDto();
        attr.setKey(key);
        attr.setName(name);
        attr.setDataType(dataType);
        attr.setDictId(dictId);
        // 默认为扩展属性（非基础字段）
        attr.setExtFlg(!"username".equals(key) && !"emailAddress".equals(key) && !"phoneNumber".equals(key)
                && !"locked".equals(key) && !"consoleAccess".equals(key) && !"enableMfa".equals(key));
        return attr;
    }
}
