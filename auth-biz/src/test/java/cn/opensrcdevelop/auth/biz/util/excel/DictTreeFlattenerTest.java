package cn.opensrcdevelop.auth.biz.util.excel;

import static org.assertj.core.api.Assertions.assertThat;

import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * DictTreeFlattener 单元测试
 *
 * <p>
 * 测试字典树扁平化功能
 */
class DictTreeFlattenerTest {

    @Test
    void flattenTree_shouldReturnFlatList_whenTreeHasSingleLevel() {
        // Given
        List<DictDataResponseDto> tree = List.of(
                createDictNode("1", "选项A", null),
                createDictNode("2", "选项B", null),
                createDictNode("3", "选项C", null));

        // When
        List<String> result = DictTreeFlattener.flattenTree(tree);

        // Then
        assertThat(result).containsExactly("选项A", "选项B", "选项C");
    }

    @Test
    void flattenTree_shouldReturnFlatListWithPath_whenTreeHasMultipleLevels() {
        // Given
        List<DictDataResponseDto> tree = List.of(
                createDictNode("1", "中国", List.of(
                        createDictNode("11", "北京", null),
                        createDictNode("12", "上海", null))),
                createDictNode("2", "美国", List.of(
                        createDictNode("21", "纽约", null),
                        createDictNode("22", "加州", List.of(
                                createDictNode("221", "洛杉矶", null),
                                createDictNode("222", "旧金山", null))))));

        // When
        List<String> result = DictTreeFlattener.flattenTree(tree);

        // Then
        assertThat(result)
                .containsExactly(
                        "中国",
                        "中国 / 北京",
                        "中国 / 上海",
                        "美国",
                        "美国 / 纽约",
                        "美国 / 加州",
                        "美国 / 加州 / 洛杉矶",
                        "美国 / 加州 / 旧金山");
    }

    @Test
    void flattenTree_shouldReturnEmptyList_whenTreeIsEmpty() {
        // Given
        List<DictDataResponseDto> tree = List.of();

        // When
        List<String> result = DictTreeFlattener.flattenTree(tree);

        // Then
        assertThat(result).isEmpty();
    }

    @Test
    void flattenTreeWithId_shouldReturnItemsWithId_whenTreeHasMultipleLevels() {
        // Given
        List<DictDataResponseDto> tree = List.of(
                createDictNode("1", "中国", List.of(
                        createDictNode("11", "北京", null),
                        createDictNode("12", "上海", null))),
                createDictNode("2", "美国", null));

        // When
        List<DictTreeFlattener.DictDisplayItem> result = DictTreeFlattener.flattenTreeWithId(tree);

        // Then
        assertThat(result)
                .hasSize(4);
        assertThat(result.get(0).id()).isEqualTo("1");
        assertThat(result.get(0).displayText()).isEqualTo("中国");
        assertThat(result.get(1).id()).isEqualTo("11");
        assertThat(result.get(1).displayText()).isEqualTo("中国 / 北京");
        assertThat(result.get(2).id()).isEqualTo("12");
        assertThat(result.get(2).displayText()).isEqualTo("中国 / 上海");
        assertThat(result.get(3).id()).isEqualTo("2");
        assertThat(result.get(3).displayText()).isEqualTo("美国");
    }

    @Test
    void flattenTreeWithId_shouldReturnEmptyList_whenTreeIsEmpty() {
        // Given
        List<DictDataResponseDto> tree = List.of();

        // When
        List<DictTreeFlattener.DictDisplayItem> result = DictTreeFlattener.flattenTreeWithId(tree);

        // Then
        assertThat(result).isEmpty();
    }

    /**
     * 创建字典数据节点
     */
    private DictDataResponseDto createDictNode(String id, String label, List<DictDataResponseDto> children) {
        DictDataResponseDto node = new DictDataResponseDto();
        node.setId(id);
        node.setLabel(label);
        node.setChildren(children);
        return node;
    }
}
