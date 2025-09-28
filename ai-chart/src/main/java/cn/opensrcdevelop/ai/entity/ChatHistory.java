package cn.opensrcdevelop.ai.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("t_chat_history")
public class ChatHistory {

    /** 对话ID */
    @TableId(type = IdType.INPUT)
    private String chatId;

    /** 用户ID */
    private String userId;

    /** 标题 */
    private String title;

    /** 描述 */
    private String description;

    /** 开始时间 */
    private LocalDateTime startTime;

    /** 结束时间 */
    private LocalDateTime endTime;
}
