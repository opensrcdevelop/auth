package cn.opensrcdevelop.auth.biz.util.excel;

import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import java.util.ArrayList;
import java.util.List;

public class DictTreeFlattener {

    private static final String SEPARATOR = " / ";

    /**
     * 将树形字典扁平化为下拉列表格式
     *
     * @param treeDict
     *            树形字典数据
     * @return 扁平化后的字典列表，格式为 "A / B / C"
     */
    public static List<String> flattenTree(List<DictDataResponseDto> treeDict) {
        List<String> result = new ArrayList<>();
        for (DictDataResponseDto node : treeDict) {
            flattenNode(node, "", result);
        }
        return result;
    }

    /**
     * 获取字典数据ID和显示文本的映射
     *
     * @param treeDict
     *            树形字典数据
     * @return ID到显示文本的映射
     */
    public static List<DictDisplayItem> flattenTreeWithId(List<DictDataResponseDto> treeDict) {
        List<DictDisplayItem> result = new ArrayList<>();
        for (DictDataResponseDto node : treeDict) {
            flattenNodeWithId(node, "", result);
        }
        return result;
    }

    private static void flattenNode(DictDataResponseDto node, String prefix, List<String> result) {
        // 先添加当前节点（无论是否有子节点）
        String currentPath = prefix.isEmpty() ? node.getLabel() : prefix + SEPARATOR + node.getLabel();
        result.add(currentPath);

        // 如果有子节点，处理子节点
        if (node.getChildren() != null && !node.getChildren().isEmpty()) {
            for (DictDataResponseDto child : node.getChildren()) {
                flattenNode(child, currentPath, result);
            }
        }
    }

    private static void flattenNodeWithId(DictDataResponseDto node, String prefix, List<DictDisplayItem> result) {
        // 先添加当前节点（无论是否有子节点）
        String currentPath = prefix.isEmpty() ? node.getLabel() : prefix + SEPARATOR + node.getLabel();
        result.add(new DictDisplayItem(node.getId(), currentPath));

        // 如果有子节点，处理子节点
        if (node.getChildren() != null && !node.getChildren().isEmpty()) {
            for (DictDataResponseDto child : node.getChildren()) {
                flattenNodeWithId(child, currentPath, result);
            }
        }
    }

    /**
     * 字典显示项
     */
    public record DictDisplayItem(String id, String displayText) {
    }
}
