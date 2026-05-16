package cn.opensrcdevelop.common.validation;

import jakarta.validation.groups.Default;

public interface ValidationGroups extends Default {

    interface Principal extends ValidationGroups {
        interface User extends Principal {
        }

        interface Role extends Principal {
        }

        interface UserGroup extends Principal {
        }
    }

    interface Operation extends ValidationGroups {
        interface INSERT extends Operation {
        }

        interface UPDATE extends Operation {
        }

        /**
         * 非 DuckDB 类型的 INSERT 校验分组（需要校验数据库连接字段）
         */
        interface INSERT_DB extends Operation {
        }
    }
}
