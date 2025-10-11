package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_model_provider")
@EntityName("模型提供商")
public class ModelProvider extends BaseEntity implements Serializable {

    /** 模型提供商ID */
    @TableId(type = IdType.INPUT)
    @PropertyName("模型提供商ID")
    private String providerId;

    /** 模型提供商名称 */
    @PropertyName("模型提供商名称")
    private String providerName;

    /** 模型提供商类型 */
    @PropertyName("模型提供商类型")
    private String providerType;

    /** 模型提供商 API URL */
    @PropertyName("模型提供商 API URL")
    private String baseUrl;

    /** 模型提供商 API Key */
    @PropertyName("模型提供商 API Key")
    private String apiKey;

    /** 可选模型 */
    @PropertyName("可选模型")
    private String optionalModels;

    /** 默认模型 */
    @PropertyName("默认模型")
    private String defaultModel;

    /** 模型温度 */
    @PropertyName("模型温度")
    private Double temperature;

    /** 模型最大令牌数 */
    @PropertyName("模型最大令牌数")
    private Integer maxTokens;

    /** 模型是否启用 */
    @PropertyName("模型是否启用")
    private Boolean enabled;
}
