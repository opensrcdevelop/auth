package cn.opensrcdevelop.auth.biz.entity.user.attr;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

@Data
@TableName("t_user_attr_mapping")
public class UserAttrMapping {

    private String userId;

    private String attrId;

    private String attrValue;
}
