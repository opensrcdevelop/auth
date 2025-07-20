package cn.opensrcdevelop.auth.audit.component;

import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.common.util.CommonUtil;
import org.springframework.stereotype.Component;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Component("linkGen")
public class LinkGenerator {

    private static final Map<ResourceType, String> LINK_TEMPLATES = new EnumMap<>(ResourceType.class);
    private static final String LINK_TEMPLATE = "<a href=\"/ui%s?id=%s\" target=\"_blank\" style=\"color: #7816ff\">%s</a>";

    static {
        LINK_TEMPLATES.put(ResourceType.USER, LINK_TEMPLATE.formatted("/user/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.USER_GROUP, LINK_TEMPLATE.formatted("/user/group/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.ROLE, LINK_TEMPLATE.formatted("/role/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.PERMISSION, LINK_TEMPLATE.formatted("/permission/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.PERMISSION_EXP, LINK_TEMPLATE.formatted("/permission/expression/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.PERMISSION_EXP_TEMPLATE, LINK_TEMPLATE.formatted("/permission/expression/template/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.CLIENT, LINK_TEMPLATE.formatted("/client/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.USER_ATTR, LINK_TEMPLATE.formatted("/user/attr/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.RESOURCE, LINK_TEMPLATE.formatted("/permission/resource/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.RESOURCE_GROUP, LINK_TEMPLATE.formatted("/resource/group/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.IDENTITY_SOURCE_PROVIDER, LINK_TEMPLATE.formatted("/identitySource/provider/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.IDENTITY_SOURCE, LINK_TEMPLATE.formatted("/identitySource/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.TENANT, LINK_TEMPLATE.formatted("/tenant/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.DICT, LINK_TEMPLATE.formatted("/dict/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.DICT_DATA, LINK_TEMPLATE.formatted("/dict/data/detail", "%s", "%s"));
        LINK_TEMPLATES.put(ResourceType.PWD_POLICY, LINK_TEMPLATE.formatted("/system_setting/password/", "%s", "%s"));
    }

    public String toLink(String id, ResourceType resourceType) {
        String template = LINK_TEMPLATES.get(resourceType);
        if (Objects.isNull(template)) {
            return id;
        }
        return template.formatted(id, id);
    }

    public List<String> toLinks(List<String> ids, ResourceType resourceType) {
        return CommonUtil.stream(ids).map(id -> toLink(id, resourceType)).toList();
    }
}
