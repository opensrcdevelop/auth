package cn.opensrcdevelop.auth.filter;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.common.filter.RestFilter;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.anji.captcha.model.common.ResponseModel;
import com.anji.captcha.model.vo.CaptchaVO;
import com.anji.captcha.service.CaptchaService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.util.AntPathMatcher;

import java.io.IOException;
import java.util.List;

@RequiredArgsConstructor
public class CaptchaVerificationCheckFilter extends RestFilter {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();
    private final List<String> includePatterns;

    private static final String REQUEST_PARAM_CAPTCHA_VERIFICATION = "captchaVerification";

    @Override
    protected void doSubFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        String path = request.getServletPath();
        // 1. 检查过滤目标的路径
        if (includePatterns.stream().anyMatch(p -> MATCHER.match(p, path))) {
            // 2. 图形验证码二次校验
            String captchaVerification = request.getParameter(REQUEST_PARAM_CAPTCHA_VERIFICATION);
            if (StringUtils.isBlank(captchaVerification)) {
                // 2.1 需要完成图形验证码验证
                WebUtil.sendJsonResponse(R.optFail(MessageConstants.LOGIN_MSG_1004, new Object()), HttpStatus.BAD_REQUEST);
                return;
            }
            CaptchaService captchaService = SpringContextUtil.getBean(CaptchaService.class);
            CaptchaVO captchaVO = new CaptchaVO();
            captchaVO.setCaptchaVerification(captchaVerification);
            ResponseModel responseModel = captchaService.verification(captchaVO);
            if (!responseModel.isSuccess()) {
                // 2.2 二次校验失败
                WebUtil.sendJsonResponse(R.optFail(MessageConstants.LOGIN_MSG_1005, new Object()), HttpStatus.BAD_REQUEST);
                return;
            }
        }
        filterChain.doFilter(request, response);
    }
}
