package cn.opensrcdevelop.ai.model;

import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class ChartRecord {

    private String chatId;

    private String questionId;

    private String question;

    private String sql;

    private List<Map<String, Object>> data;

    private List<Map<String, Object>> columns;
}
