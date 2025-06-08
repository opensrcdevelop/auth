package cn.opensrcdevelop.auth.biz.entity.system.mail;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

/**
 * 邮件模板实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_mail_template")
public class MailTemplate extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -1413117831947899892L;

    /** 模版ID */
    @TableId(type = IdType.INPUT)
    private String templateId;

    /** 模版标识 */
    private String templateCode;

    /** 模版名称 */
    private String templateName;

    /** 模版参数 */
    private String templateParameters;

    /** 模版内容 */
    private String templateContent;

    /** 主题 */
    private String subject;

    /** 发送人 */
    private String sender;

    /** 描述 */
    private String description;
}
