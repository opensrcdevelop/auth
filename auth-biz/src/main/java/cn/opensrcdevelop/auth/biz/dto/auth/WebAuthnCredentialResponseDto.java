package cn.opensrcdevelop.auth.biz.dto.auth;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * WebAuthn 凭证响应 DTO
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WebAuthnCredentialResponseDto {

    /**
     * 凭证 ID（截断显示）
     */
    private String id;

    /**
     * 凭证 ID 前缀（用于识别）
     */
    private String idPrefix;

    /**
     * 设备类型
     */
    private String deviceType;

    /**
     * 传输类型列表
     */
    private String[] transports;

    /**
     * 创建时间
     */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime createdAt;

    /**
     * 最后使用时间
     */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime lastUsedAt;

    /**
     * 是否可删除
     */
    private Boolean deletable;
}
