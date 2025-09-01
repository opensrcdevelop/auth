package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.ai.entity.ChartConf;
import cn.opensrcdevelop.common.exception.ServerException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import lombok.experimental.UtilityClass;

import java.util.*;

@UtilityClass
public class ChartRenderer {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * 根据 ChartConf 渲染图表
     *
     * @param conf     配置实体
     * @param dataRows 查询结果
     * @return 图表
     */
    public Tuple2<String, Object> render(ChartConf conf, List<Map<String, Object>> dataRows) {
        try {
            JsonNode cfg = MAPPER.readTree(conf.getConfig());
            String displayType = cfg.path("displayType").asText("chart");

            if ("table".equalsIgnoreCase(displayType)) {
                return Tuple.of("table", buildArcoTableConfig(cfg, dataRows));
            } else {
                return Tuple.of("chart", buildEchartsOption(cfg, dataRows));
            }
        } catch (Exception e) {
            throw new ServerException("渲染图表失败", e);
        }
    }

    private Map<String, Object> buildArcoTableConfig(JsonNode cfg, List<Map<String, Object>> rows) {
        JsonNode columnsNode = cfg.path("fieldMapping").path("columns");
        String titleText = cfg.path("meta").path("title").asText("");
        String description = cfg.path("meta").path("description").asText("");

        // 1. 标题
        Map<String, Object> title = Map.of(
                "text", titleText,
                "description", description
        );

        // 无数据
        if (!columnsNode.isArray() || columnsNode.isEmpty() || rows.isEmpty()) {
            return Map.of(
                    "columns", List.of(),
                    "data", List.of(),
                    "title", title
            );
        }

        // 2. 列配置
        List<Map<String, Object>> columns = MAPPER.convertValue(columnsNode, new TypeReference<>() {});
        List<Map<String, Object>> arcoColumns = new ArrayList<>();
        for (Map<String, Object> col : columns) {
            Map<String, Object> column = new HashMap<>();
            // 2.1 列标题
            column.put("title", col.get("title"));
            // 2.2 列信息的标识
            column.put("dataIndex", col.get("key"));
            // 2.3 对齐方式
            column.put("align", "left");
            // 2.4 排序
            column.put("sortable", Map.of(
                    "sortDirections", List.of("ascend", "descend")
            ));
            arcoColumns.add(column);
        }

        // 3. 表格配置
        Map<String, Object> tableConfig = new HashMap<>();
        tableConfig.put("columns", arcoColumns);
        tableConfig.put("data", rows);
        tableConfig.put("title", title);

        return tableConfig;
    }

    private Map<String, Object> buildEchartsOption(JsonNode cfg, List<Map<String, Object>> rows) {
        // 1. 基础信息
        String chartType = cfg.path("chartType").asText("bar");
        String titleText = cfg.path("meta").path("title").asText("");
        String description = cfg.path("meta").path("description").asText("");

        // 2. options 节点
        JsonNode opt = cfg.path("options");
        boolean smooth = opt.path("smooth").asBoolean(false);
        boolean legend = opt.path("legend").asBoolean(true);

        // 3. 坐标轴标题
        JsonNode axisName = opt.path("axisName");
        String xName = axisName.path("x").asText("");
        String yName = axisName.path("y").asText("");

        // 4. 字段映射
        String xKey = cfg.path("fieldMapping").path("xAxis").asText(null);
        String yKey = cfg.path("fieldMapping").path("yAxis").asText(null);

        // 5. 公共 title
        Map<String, Object> title = Map.of(
                "text", titleText,
                "description", description
        );

        // 6. 图例
        Map<String, Object> legendMap = Map.of("show", legend, "data", List.of());

        // 7. 工具箱
        Map<String, Object> toolboxFeature = new LinkedHashMap<>();
        toolboxFeature.put("restore", Map.of("show", true));
        toolboxFeature.put("saveAsImage", Map.of(
                "show", true,
                "type", "png",
                "name", titleText,
                "backgroundColor", "#fff",
                "pixelRatio", 2
        ));
        toolboxFeature.put("magicType", Map.of(
                "show", true,
                "type", List.of("line", "bar")
        ));
        Map<String, Object> toolboxMap = Map.of(
                "show", true,
                "orient", "horizontal",
                "itemSize", 15,
                "itemGap", 10,
                "left", "right",
                "top", "yop",
                "feature", toolboxFeature
        );

        // 9. 系列数据
        List<Map<String, Object>> seriesData;
        Map<String, Object> xAxis = null;
        Map<String, Object> yAxis = null;

        // 9.1 饼图/漏斗图特殊处理
        if ("pie".equalsIgnoreCase(chartType) || "funnel".equalsIgnoreCase(chartType)) {
            seriesData = rows.stream()
                    .map(r -> Map.of(
                            "name", r.get(xKey),
                            "value", r.get(yKey)
                    ))
                    .toList();
        } else {
            // 9.2 折线 / 柱形 / 雷达 / 散点
            List<Object> xData = extractColumn(rows, xKey);
            List<Object> yData = extractColumn(rows, yKey);

            xAxis = Map.of(
                    "type", "category",
                    "name", xName,
                    "data", xData
            );
            yAxis = Map.of(
                    "type", "value",
                    "name", yName
            );

            Map<String, Object> series = Map.ofEntries(
                    Map.entry("name", titleText),
                    Map.entry("type", chartType),
                    Map.entry("data", yData),
                    Map.entry("smooth", smooth)
            );
            seriesData = List.of(series);
        }

        Map<String, Object> option = new LinkedHashMap<>();
        option.put("tooltip", Map.of("trigger", "axis"));
        option.put("legend", legendMap);
        option.put("toolbox", toolboxMap);
        option.put("xAxis", xAxis);
        option.put("yAxis", yAxis);
        option.put("series", seriesData);

        return Map.of("title", title, "option", option);
    }

    private List<Object> extractColumn(List<Map<String, Object>> rows, Object key) {
        return rows.stream()
                .map(r -> r.get(String.valueOf(key)))
                .toList();
    }
}
