package cn.opensrcdevelop.auth.biz.util.excel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Data;

public class ExcelFieldMappingParser {

    /**
     * 解析字段映射信息
     *
     * @param hiddenSheetRows
     *            隐藏 sheet 的行数据
     * @return 字段映射信息
     */
    public static FieldMappingInfo parseFieldMapping(List<List<String>> hiddenSheetRows) {
        Map<String, FieldMapping> keyToMapping = new HashMap<>();
        List<FieldMapping> basicFields = new ArrayList<>();
        List<FieldMapping> extFields = new ArrayList<>();

        boolean foundFieldSection = false;
        for (List<String> row : hiddenSheetRows) {
            if (row.size() < 3) {
                continue;
            }
            String firstCol = row.get(0);
            if ("字段Key".equals(firstCol)) {
                foundFieldSection = true;
                continue;
            }
            if ("字典ID".equals(firstCol)) {
                break; // 到达字典数据部分
            }
            if (foundFieldSection && !firstCol.isEmpty()) {
                FieldMapping mapping = new FieldMapping();
                mapping.setKey(row.get(0));
                mapping.setName(row.get(1));
                mapping.setDataType(row.get(2));
                mapping.setDictId(row.size() > 3 ? row.get(3) : null);
                keyToMapping.put(mapping.getKey(), mapping);

                // 区分基础字段和扩展字段
                if ("username".equals(mapping.getKey()) || "emailAddress".equals(mapping.getKey())
                        || "phoneNumber".equals(mapping.getKey()) || "locked".equals(mapping.getKey())
                        || "consoleAccess".equals(mapping.getKey()) || "enableMfa".equals(mapping.getKey())) {
                    basicFields.add(mapping);
                } else {
                    extFields.add(mapping);
                }
            }
        }

        return new FieldMappingInfo(keyToMapping, basicFields, extFields);
    }

    /**
     * 解析字典数据映射
     *
     * @param hiddenSheetRows
     *            隐藏 sheet 的行数据
     * @return 字典数据映射 (dictId -> (dataId -> displayText))
     */
    public static Map<String, Map<String, String>> parseDictMapping(List<List<String>> hiddenSheetRows) {
        Map<String, Map<String, String>> dictMapping = new HashMap<>();
        boolean foundDictSection = false;

        for (List<String> row : hiddenSheetRows) {
            if (row.size() < 3) {
                continue;
            }
            String firstCol = row.get(0);
            if ("字典ID".equals(firstCol)) {
                foundDictSection = true;
                continue;
            }
            if (foundDictSection && !firstCol.isEmpty()) {
                String dictId = row.get(0);
                String dataId = row.get(1);
                String displayText = row.get(2);

                dictMapping.computeIfAbsent(dictId, k -> new HashMap<>()).put(dataId, displayText);
            }
        }

        return dictMapping;
    }

    /**
     * 根据显示文本查找字典数据ID
     *
     * @param dictId
     *            字典ID
     * @param displayText
     *            显示文本（格式："A / B"）
     * @param dictMapping
     *            字典映射
     * @return 字典数据ID
     */
    public static String findDictDataId(String dictId, String displayText,
            Map<String, Map<String, String>> dictMapping) {
        Map<String, String> dictDataMap = dictMapping.get(dictId);
        if (dictDataMap == null) {
            return null;
        }

        // 从显示文本中提取最后一个部分（叶子节点的标签）
        String[] parts = displayText.split(" / ");
        String leafLabel = parts[parts.length - 1];

        // 查找匹配的数据ID
        for (Map.Entry<String, String> entry : dictDataMap.entrySet()) {
            if (entry.getValue().equals(leafLabel)) {
                return entry.getKey();
            }
        }

        return null;
    }

    @Data
    public static class FieldMappingInfo {
        private Map<String, FieldMapping> keyToMapping;
        private List<FieldMapping> basicFields;
        private List<FieldMapping> extFields;

        public FieldMappingInfo(Map<String, FieldMapping> keyToMapping, List<FieldMapping> basicFields,
                List<FieldMapping> extFields) {
            this.keyToMapping = keyToMapping;
            this.basicFields = basicFields;
            this.extFields = extFields;
        }
    }

    @Data
    public static class FieldMapping {
        private String key;
        private String name;
        private String dataType;
        private String dictId;
    }
}
