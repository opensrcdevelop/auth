package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.ai.entity.ChartConf;
import cn.opensrcdevelop.common.exception.ServerException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.experimental.UtilityClass;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@UtilityClass
public class ChartRenderer {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * 根据 ChartConf 渲染图表
     *
     * @param conf     配置实体
     * @param dataRows 查询结果
     * @return ECharts option JSON 或 Markdown 表格
     */
    public Object render(ChartConf conf, List<Map<String, Object>> dataRows) {
        try {
            JsonNode cfg = MAPPER.readTree(conf.getConfig());
            String displayType = cfg.path("displayType").asText("chart");

            if ("table".equalsIgnoreCase(displayType)) {
                return buildMarkdownTable(cfg, dataRows);
            } else {
                return buildEchartsOption(cfg, dataRows);
            }
        } catch (Exception e) {
            throw new ServerException("渲染图表失败", e);
        }
    }

    private String buildMarkdownTable(JsonNode cfg, List<Map<String, Object>> rows) {
        JsonNode columnsNode = cfg.path("fieldMapping").path("columns");
        if (!columnsNode.isArray() || columnsNode.isEmpty() || rows.isEmpty()) {
            return "**暂无数据**";
        }

        List<Map<String, Object>> columns = MAPPER.convertValue(columnsNode, new TypeReference<>() {
        });

        // 表头
        StringBuilder sb = new StringBuilder("|");
        for (Map<String, Object> col : columns) {
            sb.append(col.get("title")).append('|');
        }
        sb.append('\n');

        // 分隔符
        sb.append("|");
        for (Map<String, Object> col : columns) {
            String align = String.valueOf(col.getOrDefault("align", "left"));
            sb.append(alignSep(align)).append('|');
        }
        sb.append('\n');

        // 数据行
        for (Map<String, Object> row : rows) {
            sb.append("|");
            for (Map<String, Object> col : columns) {
                String key = String.valueOf(col.get("key"));
                Object val = row.getOrDefault(key, "");
                boolean markdown = "markdown".equals(col.get("render"));
                String cell = markdown ? String.valueOf(val) : escapeMd(String.valueOf(val));
                sb.append(cell).append('|');
            }
            sb.append('\n');
        }
        return sb.toString();
    }

    private Map<String, Object> buildEchartsOption(JsonNode cfg, List<Map<String, Object>> rows) {
        // 1. 基础信息
        String chartType = cfg.path("chartType").asText("bar");
        String titleText = cfg.path("meta").path("title").asText("");
        String subtitle = cfg.path("meta").path("description").asText("");

        // 2. options 节点
        JsonNode opt = cfg.path("options");
        boolean smooth = opt.path("smooth").asBoolean(false);
        boolean legend = opt.path("legend").asBoolean(true);
        boolean toolbox = opt.path("toolbox").asBoolean(false);
        String unit = opt.path("unit").asText("");

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
                "subtext", subtitle
        );

        // 6. 图例
        Map<String, Object> legendMap = Map.of("show", legend, "data", List.of());

        // 7. 工具箱
        Map<String, Object> toolboxMap = toolbox
                ? Map.of("feature", Map.of("saveAsImage", Map.of()))
                : null;

        // 8. 坐标轴格式化
        String axisFmt = unit.isEmpty() ? "{value}" : "{value}" + unit;

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
                    "data", xData,
                    "axisLabel", Map.of("formatter", axisFmt)
            );
            yAxis = Map.of(
                    "type", "value",
                    "name", yName,
                    "axisLabel", Map.of("formatter", axisFmt)
            );

            Map<String, Object> series = Map.ofEntries(
                    Map.entry("name", titleText),
                    Map.entry("type", chartType),
                    Map.entry("data", yData),
                    Map.entry("smooth", smooth)
            );
            seriesData = List.of(series);
        }

        Map<String, Object> option = new HashMap<>();
        option.put("title", title);
        option.put("tooltip", Map.of("trigger", "axis"));
        option.put("legend", legendMap);
        option.put("toolbox", toolboxMap);
        option.put("xAxis", xAxis);
        option.put("yAxis", yAxis);
        option.put("series", seriesData);

        return option;
    }

    private String alignSep(String align) {
        return switch (align) {
            case "center" -> ":-:|";
            case "right" -> "-:|";
            default -> ":--|";
        };
    }

    private String escapeMd(String s) {
        return s.replace("|", "\\|")
                .replace("\n", "<br/>");
    }

    private List<Object> extractColumn(List<Map<String, Object>> rows, Object key) {
        return rows.stream()
                .map(r -> r.get(String.valueOf(key)))
                .toList();
    }
}
