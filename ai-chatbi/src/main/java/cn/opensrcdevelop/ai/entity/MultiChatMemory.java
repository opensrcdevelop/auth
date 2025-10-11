package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("t_multi_chat_memory")
@EntityName("多轮对话记忆")
public class MultiChatMemory {

    @TableId(type = IdType.AUTO)
    @PropertyName("主键")
    private Long id;

    /** 对话ID */
    @PropertyName("对话ID")
    private String chatId;

    /** 提示词模板 */
    @PropertyName("提示词模板")
    private String promptTemplate;

    /** 内容 */
    @PropertyName("内容")
    private String content;

    /** 类型 */
    @PropertyName("类型")
    private String type;

    /** 创建时间 */
    @PropertyName("创建时间")
    private LocalDateTime createTime;
}
