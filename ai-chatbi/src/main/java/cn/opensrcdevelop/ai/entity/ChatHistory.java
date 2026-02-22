package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.time.LocalDateTime;
import lombok.Data;

@Data
@TableName("t_chat_history")
@EntityName("对话历史")
public class ChatHistory {

    /** 对话ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("对话ID")
    private String chatId;

    /** 用户ID */
    @PropertyName("用户ID")
    private String userId;

    /** 数据源ID */
    @PropertyName("数据源ID")
    private String dataSourceId;

    /** 标题 */
    @PropertyName("标题")
    private String title;

    /** 描述 */
    @PropertyName("描述")
    private String description;

    /** 开始时间 */
    @PropertyName("开始时间")
    private LocalDateTime startTime;

    /** 结束时间 */
    @PropertyName("结束时间")
    private LocalDateTime endTime;
}
