package cn.opensrcdevelop.auth.audit.enums;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum ResourceEnum {

    AUTHZ_COND("14ee7b7e-db4c-40cc-b93e-d38969be5542", "授权条件"),
    CLIENT("75a3dee9-a95f-4ad3-a32a-a7f6d34c0050", "客户端"),
    DICT("22c26616-ba71-4258-a9b4-0901cc3285b7", "字典"),
    DICT_DATA("ab15084d-285e-4a2d-bde2-d968a504e7a5", "字典数据"),
    OIDC_CLAIM("931848a2-beb6-444e-a7d7-72e909553b00", "OIDC Claim"),
    OIDC_SCOPE("0f35efeb-3f5a-4e22-84c4-4b4a08b6717c", "OIDC Scope"),
    PWD_POLICY("2d2225a8-5143-4465-ab76-06b101abff8f", "密码策略"),
    PERMISSION("75c35dc3-1996-48ab-be27-e4078f86a559", "权限"),
    PERMISSION_EXP("79a30d3a-0fde-4087-a307-619cc0c56b17", "权限表达式"),
    RESOURCE("edd2a541-f482-45cd-9842-c1ebf43c346c", "资源"),
    RESOURCE_GROUP("911e08a0-d91a-4c66-8a7d-c8fda2c79c69", "资源组"),
    ROLE("4d367bc0-d043-402c-a1d5-d4e5c55c9e23", "角色"),
    USER("97392350-5214-4dbb-83e8-45b678ce145e", "用户"),
    USER_ATTR("da2c6573-d236-4e4d-96a4-85c517b72c59", "用户属性"),
    USER_GROUP("1624ca73-e656-48d9-800e-b5762b51d7c5", "用户组"),
    TENANT("055a8115-8be2-498e-a6f9-ab8a34f5dd0e", "租户");


    private final String id;

    private final String name;
}
