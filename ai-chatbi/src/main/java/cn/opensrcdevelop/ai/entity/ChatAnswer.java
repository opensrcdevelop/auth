package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.*;
import com.baomidou.mybatisplus.annotation.TableField;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_chat_answer")
public class ChatAnswer extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 3772087736568681535L;

    @TableId(type = IdType.INPUT)
    private String answerId;

    private String dataSourceId;

    private String modelProviderId;

    private String model;

    private String questionId;

    private String chatId;

    private String question;

    private String answer;

    private String sql;

    private String chartConfig;

    private String reportType;

    private String report;

    @TableField(updateStrategy = FieldStrategy.ALWAYS)
    private String feedback;
}
