package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.ai.enums.Feedback;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class VoteAnswerRequestDto {

    @NotNull
    private String answerId;

    private Feedback feedback;
}
