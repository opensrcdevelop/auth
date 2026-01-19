package cn.opensrcdevelop.auth.biz.util.excel;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

/**
 * ExcelFieldMappingParser 单元测试
 *
 * <p>
 * 测试隐藏 sheet 字段映射信息解析功能
 */
class ExcelFieldMappingParserTest {

    @Test
    void parseFieldMapping_shouldReturnBasicAndExtFields_whenInputHasValidData() {
        // Given
        List<List<String>> hiddenSheetRows = List.of(
                List.of("字段Key", "字段名称", "数据类型", "字典ID"),
                List.of("username", "用户名", "STRING", ""),
                List.of("emailAddress", "邮箱", "STRING", ""),
                List.of("department", "部门", "DICT", "dept_dict"),
                List.of("level", "级别", "NUMBER", ""),
                List.of("字典ID", "数据ID", "显示文本"),
                List.of("dept_dict", "1", "技术部"),
                List.of("dept_dict", "2", "销售部"));

        // When
        ExcelFieldMappingParser.FieldMappingInfo result = ExcelFieldMappingParser.parseFieldMapping(hiddenSheetRows);

        // Then
        assertThat(result.getKeyToMapping()).hasSize(4);
        assertThat(result.getBasicFields()).hasSize(2);
        assertThat(result.getExtFields()).hasSize(2);

        // 验证基础字段 key
        assertThat(result.getBasicFields())
                .extracting("key")
                .containsExactlyInAnyOrder("username", "emailAddress");

        // 验证扩展字段 key
        assertThat(result.getExtFields())
                .extracting("key")
                .containsExactlyInAnyOrder("department", "level");
    }

    @Test
    void parseFieldMapping_shouldHandleMissingDictIdColumn_whenInputHasOnlyThreeColumns() {
        // Given
        List<List<String>> hiddenSheetRows = List.of(
                List.of("字段Key", "字段名称", "数据类型"),
                List.of("username", "用户名", "STRING"),
                List.of("emailAddress", "邮箱", "STRING"));

        // When
        ExcelFieldMappingParser.FieldMappingInfo result = ExcelFieldMappingParser.parseFieldMapping(hiddenSheetRows);

        // Then
        assertThat(result.getKeyToMapping()).hasSize(2);
        assertThat(result.getBasicFields().get(0).getDictId()).isNull();
    }

    @Test
    void parseFieldMapping_shouldSkipInvalidRows_whenInputHasInsufficientColumns() {
        // Given
        List<List<String>> hiddenSheetRows = List.of(
                List.of("字段Key", "字段名称", "数据类型"),
                List.of("username", "用户名"),
                List.of("emailAddress", "邮箱", "STRING", "DICT", "extra_column"));

        // When
        ExcelFieldMappingParser.FieldMappingInfo result = ExcelFieldMappingParser.parseFieldMapping(hiddenSheetRows);

        // Then
        assertThat(result.getKeyToMapping()).hasSize(1);
        assertThat(result.getKeyToMapping()).containsKey("emailAddress");
        assertThat(result.getKeyToMapping()).doesNotContainKey("username");
    }

    @Test
    void parseFieldMapping_shouldReturnEmptyMapping_whenInputHasNoFieldData() {
        // Given
        List<List<String>> hiddenSheetRows = List.of(
                List.of("字段Key", "字段名称", "数据类型"),
                List.of("字典ID", "数据ID", "显示文本"));

        // When
        ExcelFieldMappingParser.FieldMappingInfo result = ExcelFieldMappingParser.parseFieldMapping(hiddenSheetRows);

        // Then
        assertThat(result.getKeyToMapping()).isEmpty();
        assertThat(result.getBasicFields()).isEmpty();
        assertThat(result.getExtFields()).isEmpty();
    }

    @Test
    void parseDictMapping_shouldReturnCorrectMapping_whenInputHasValidDictData() {
        // Given
        List<List<String>> hiddenSheetRows = List.of(
                List.of("字段Key", "字段名称", "数据类型"),
                List.of("字典ID", "数据ID", "显示文本"),
                List.of("dept_dict", "1", "技术部"),
                List.of("dept_dict", "2", "销售部"),
                List.of("level_dict", "L1", "初级"),
                List.of("level_dict", "L2", "高级"));

        // When
        Map<String, Map<String, String>> result = ExcelFieldMappingParser.parseDictMapping(hiddenSheetRows);

        // Then
        assertThat(result).hasSize(2);
        assertThat(result.get("dept_dict"))
                .containsExactlyInAnyOrderEntriesOf(Map.of("1", "技术部", "2", "销售部"));
        assertThat(result.get("level_dict"))
                .containsExactlyInAnyOrderEntriesOf(Map.of("L1", "初级", "L2", "高级"));
    }

    @Test
    void parseDictMapping_shouldReturnEmptyMapping_whenInputHasNoDictData() {
        // Given
        List<List<String>> hiddenSheetRows = List.of(
                List.of("字段Key", "字段名称", "数据类型"),
                List.of("username", "用户名", "STRING"));

        // When
        Map<String, Map<String, String>> result = ExcelFieldMappingParser.parseDictMapping(hiddenSheetRows);

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    void findDictDataId_shouldReturnCorrectId_whenDisplayTextMatches() {
        // Given
        Map<String, Map<String, String>> dictMapping = Map.of(
                "dept_dict", Map.of("1", "技术部", "2", "销售部"));

        // When
        String result = ExcelFieldMappingParser.findDictDataId("dept_dict", "技术部", dictMapping);

        // Then
        assertThat(result).isEqualTo("1");
    }

    @Test
    void findDictDataId_shouldReturnCorrectId_whenDisplayTextHasPathSeparator() {
        // Given
        // dictMapping 结构是: dictId -> (dataId -> leafLabel)
        Map<String, Map<String, String>> dictMapping = Map.of(
                "area_dict", Map.of("1", "朝阳", "2", "上海"));

        // When
        // displayText 是完整路径 "中国 / 上海"，会提取叶子节点 "上海" 进行匹配
        String result = ExcelFieldMappingParser.findDictDataId("area_dict", "中国 / 上海", dictMapping);

        // Then
        assertThat(result).isEqualTo("2");
    }

    @Test
    void findDictDataId_shouldReturnNull_whenDisplayTextNotFound() {
        // Given
        Map<String, Map<String, String>> dictMapping = Map.of(
                "dept_dict", Map.of("1", "技术部", "2", "销售部"));

        // When
        String result = ExcelFieldMappingParser.findDictDataId("dept_dict", "市场部", dictMapping);

        // Then
        assertThat(result).isNull();
    }

    @Test
    void findDictDataId_shouldReturnNull_whenDictIdNotFound() {
        // Given
        Map<String, Map<String, String>> dictMapping = Map.of(
                "dept_dict", Map.of("1", "技术部"));

        // When
        String result = ExcelFieldMappingParser.findDictDataId("unknown_dict", "技术部", dictMapping);

        // Then
        assertThat(result).isNull();
    }
}
