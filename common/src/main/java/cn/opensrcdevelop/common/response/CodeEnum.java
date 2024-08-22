package cn.opensrcdevelop.common.response;

import cn.opensrcdevelop.common.util.MessageUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import lombok.Getter;

@Getter
public enum CodeEnum {

    // 操作成功
    RCD0(0),
    // 系统内部错误
    RCD50000(50000),
    // 操作失败
    RCD20000(20000),
    // 参数检验未通过
    RCD20001(20001),
    // 未认证
    RCD40001(40001),
    // 未授权
    RCD40003(40003),
    // 404
    RCD40004(40004),
    // 存在重复数据
    RCD40005(40005);

    // 响应码
    private final Integer code;

    // 消息
    private final String message;

    CodeEnum(Integer code) {
        this.code = code;
        MessageUtil messageUtil = SpringContextUtil.getBean(MessageUtil.class);
        this.message = messageUtil.getMsg(String.valueOf(code));
    }
}
