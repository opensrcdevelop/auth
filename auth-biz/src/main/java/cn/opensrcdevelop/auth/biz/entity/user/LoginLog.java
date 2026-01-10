package cn.opensrcdevelop.auth.biz.entity.user;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;

@Data
@TableName("t_login_log")
public class LoginLog implements Serializable {

    @Serial
    private static final long serialVersionUID = 8239998088007565184L;

    /** 登录ID */
    @TableId(type = IdType.INPUT)
    private String loginId;

    /** 用户ID */
    private String userId;

    /** 会话ID */
    private String sessionId;

    /** 客户端ID */
    private String clientId;

    /** 登录IP */
    private String loginIp;

    /** 登录IP属地 */
    private String loginIpRegion;

    /** 设备类型 */
    private String deviceType;

    /** 设备OS */
    private String deviceOs;

    /** 浏览器类型 */
    private String browserType;

    /** 登录时间 */
    private LocalDateTime loginTime;
}
