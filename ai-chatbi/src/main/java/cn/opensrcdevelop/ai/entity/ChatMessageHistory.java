package cn.opensrcdevelop.ai.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("t_chat_message_history")
public class ChatMessageHistory {

    /** 消息ID */
    @TableId(type = IdType.INPUT)
    private String messageId;

    /** 角色 */
    private String role;

    /** 类型 */
    private String type;

    /** 内容 */
    private String content;

    /** 问题ID */
    private String questionId;

    /** 对话ID */
    private String chatId;

    /** 回答ID */
    private String answerId;

    /** 重写后的问题 */
    private String rewrittenQuestion;

     /** 时间 */
    private LocalDateTime time;

    /** 用户ID */
    private String userId;

    /** 创建时间 */
    private LocalDateTime createTime;
}
