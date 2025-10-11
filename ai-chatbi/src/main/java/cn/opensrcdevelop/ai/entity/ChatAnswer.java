package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
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
@EntityName("对话回答")
public class ChatAnswer extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 3772087736568681535L;

    @TableId(type = IdType.INPUT)
    @PropertyName("回答ID")
    private String answerId;

    @PropertyName("数据源ID")
    private String dataSourceId;

    @PropertyName("模型提供商ID")
    private String modelProviderId;

    @PropertyName("模型")
    private String model;

    @PropertyName("问题ID")
    private String questionId;

    @PropertyName("对话ID")
    private String chatId;

    @PropertyName("问题")
    private String question;

    @PropertyName("回答")
    private String answer;

    @PropertyName("生成的 SQL")
    private String sql;

    @PropertyName("图表配置")
    private String chartConfig;

    @PropertyName("报表类型")
    private String reportType;

    @PropertyName("报表")
    private String report;

    @PropertyName("请求消耗的 token 数")
    private Integer reqTokens;

    @PropertyName("回复消耗的 token 数")
    private Integer repTokens;

    @TableField(updateStrategy = FieldStrategy.ALWAYS)
    @PropertyName("用户反馈")
    private String feedback;
}
