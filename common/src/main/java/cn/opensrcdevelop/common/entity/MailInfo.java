package cn.opensrcdevelop.common.entity;

import java.util.List;
import lombok.Data;

/**
 * 邮件信息
 */
@Data
public class MailInfo {

    /** 主题 */
    private String subject;

    /** 发件人 */
    private String from;

    /** 收件人 */
    private List<String> to;

    /** 抄送 */
    private List<String> cc;

    /** 密送 */
    private List<String> bcc;

    /** 邮件内容 */
    private String content;
}
