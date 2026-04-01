package cn.opensrcdevelop.ai.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

/**
 * ChatBI 对话配置 DTO
 */
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class ChatConfigDto {

    /** 最大思考步数 */
    private Integer maxSteps = 30;

    /** 回答语言 */
    private String language = "简体中文";

    /** API 重试次数 */
    private Integer apiRetryCount = 3;
}
