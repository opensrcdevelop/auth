package cn.opensrcdevelop.ai.model;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

@Data
@SuppressWarnings("java:S1948")
public class ChartRecord implements Serializable {

    @Serial
    private static final long serialVersionUID = 7928792978296643129L;

    private String chatId;

    private String questionId;

    private String question;

    private String sql;

    private List<Map<String, Object>> data;

    private List<Map<String, Object>> columns;
}
