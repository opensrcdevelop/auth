package cn.opensrcdevelop.auth.biz.service.permission.expression;

import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamConfigDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionExpTemplate;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Map;

public interface PermissionExpTemplateService extends IService<PermissionExpTemplate> {

    void createPermissionExpTemplate(PermissionExpTemplateRequestDto requestDto);

    void updatePermissionExpTemplate(PermissionExpTemplateRequestDto requestDto);

    void deletePermissionExpTemplate(String templateId);

    List<PermissionExpTemplateParamConfigDto> getParamsConfigs(String templateId);

    PageData<PermissionExpTemplateResponseDto> list(int page, int size, String keyword);

    PermissionExpTemplateResponseDto detail(String templateId);

    List<PermissionExpResponseDto> getPermissionExpList(String templateId);

    Map<String, Object> getParamExecutionContext(List<PermissionExpTemplateParamConfigDto> paramConfigs,
            List<PermissionExpTemplateParamDto> params);
}
