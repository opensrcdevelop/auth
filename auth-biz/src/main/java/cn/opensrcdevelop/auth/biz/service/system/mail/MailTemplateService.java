package cn.opensrcdevelop.auth.biz.service.system.mail;

import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateRequestDto;
import cn.opensrcdevelop.auth.biz.dto.system.mail.MailTemplateResponseDto;
import cn.opensrcdevelop.auth.biz.entity.system.mail.MailTemplate;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface MailTemplateService extends IService<MailTemplate> {

    List<MailTemplateResponseDto> templateList();

    MailTemplateResponseDto detail(String templateId);

    void update(MailTemplateRequestDto requestDto);

    MailTemplate getByCode(String code);
}
