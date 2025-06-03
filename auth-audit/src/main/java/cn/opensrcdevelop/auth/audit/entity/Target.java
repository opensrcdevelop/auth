package cn.opensrcdevelop.auth.audit.entity;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

@Data
public class Target implements Serializable {

    @Serial
    private static final long serialVersionUID = -3554773092670667648L;

    private Object before;

    private Object after;
}
