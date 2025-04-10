package cn.opensrcdevelop.auth.biz.service.system.password;

import cn.opensrcdevelop.auth.biz.dto.system.password.*;
import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicy;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface PasswordPolicyService extends IService<PasswordPolicy> {

    List<PasswordPolicyResponseDto> getList();

    void createPasswordPolicy (PasswordPolicyRequestDto requestDto);

    PasswordPolicyResponseDto detail(String id);

    void updatePasswordPolicyPriority(List<UpdatePasswordPolicyPriorityRequestDto> requestDtoList);

    void updatePasswordPolicy(PasswordPolicyRequestDto requestDto);

    void deletePasswordPolicy(String id);

    CheckPasswordStrengthResponseDto checkPasswordStrength(CheckPasswordStrengthWithPolicyRequestDto requestDto);

    CheckPasswordStrengthResponseDto checkPasswordStrength(CheckPasswordStrengthRequestDto requestDto);

    boolean checkLoginPasswordStrength(String userId, String password);

    void checkPasswordStrength(String userId, String password);
}
