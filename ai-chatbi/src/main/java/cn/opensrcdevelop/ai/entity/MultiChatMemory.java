package cn.opensrcdevelop.ai.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDateTime;

@Data
@TableName("t_multi_chat_memory")
public class MultiChatMemory {

    @TableId(type = IdType.AUTO)
    private Long id;

    /** 对话ID */
    private String chatId;

    /** 提示词模板 */
    private String promptTemplate;

    /** 内容 */
    private String content;

    /** 类型 */
    private String type;

    /** 创建时间 */
    private LocalDateTime createTime;
}
