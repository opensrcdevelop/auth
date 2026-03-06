package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
public class UserAnswerDto implements Serializable {

    @Serial
    private static final long serialVersionUID = 7036766180785462234L;

    @Schema(description = "问题ID")
    @NotBlank
    private String questionId;

    @Schema(description = "用户回答")
    @NotBlank
    private String answer;
}
