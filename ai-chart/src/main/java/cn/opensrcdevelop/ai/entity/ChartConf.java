package cn.opensrcdevelop.ai.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.*;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_chart_conf")
public class ChartConf extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 3772087736568681535L;

    @TableId(type = IdType.INPUT)
    private String chartId;

    private String dataSourceId;

    private String questionId;

    private String chatId;

    private String question;

    private String config;

    private String sql;

    private String modelProviderId;

    private String model;

    @TableField(insertStrategy = FieldStrategy.ALWAYS, updateStrategy = FieldStrategy.ALWAYS)
    private String feedBack;
}
