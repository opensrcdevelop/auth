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
    }
}
