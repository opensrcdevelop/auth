package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.time.LocalDateTime;
import lombok.Data;

@Data
@TableName("t_chat_message_history")
@EntityName("对话消息历史")
public class ChatMessageHistory {

    /** 消息ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("消息ID")
    private String messageId;

    /** 角色 */
    @PropertyName("消息角色")
    private String role;

    /** 类型 */
    @PropertyName("消息类型")
    private String type;

    /** 内容 */
    @PropertyName("消息内容")
    private String content;

    /** 问题ID */
    @PropertyName("问题ID")
    private String questionId;

    /** 对话ID */
    @PropertyName("对话ID")
    private String chatId;

    /** 回答ID */
    @PropertyName("回答ID")
    private String answerId;

    /** 重写后的问题 */
    @PropertyName("重写后的问题")
    private String rewrittenQuestion;

    /** 输入 token 数 */
    @PropertyName("输入 token 数")
    private Long inputTokens;

    /** 输出 token 数 */
    @PropertyName("输出 token 数")
    private Long outputTokens;

    /** 时间 */
    @PropertyName("时间")
    private LocalDateTime time;

    /** 用户ID */
    @PropertyName("用户ID")
    private String userId;

    /** 模型提供商ID */
    @PropertyName("模型提供商ID")
    private String modelProviderId;

    /** 模型 */
    @PropertyName("模型")
    private String model;

    /** 创建时间 */
    @PropertyName("创建时间")
    private LocalDateTime createTime;
}
