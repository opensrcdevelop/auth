package cn.opensrcdevelop.common.controller;

import cn.opensrcdevelop.common.annoation.NoPathPrefix;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.boot.autoconfigure.web.ErrorProperties;
import org.springframework.boot.autoconfigure.web.servlet.error.BasicErrorController;
import org.springframework.boot.web.error.ErrorAttributeOptions;
import org.springframework.boot.web.servlet.error.DefaultErrorAttributes;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * 处理过滤器抛出的异常
 */
@NoPathPrefix
@RestController
public class ExceptionController extends BasicErrorController {

    public ExceptionController() {
        super(new DefaultErrorAttributes(), new ErrorProperties());
    }

    @RequestMapping(produces = { MediaType.APPLICATION_JSON_VALUE, MediaType.TEXT_HTML_VALUE })
    public void errorHandler(HttpServletRequest request) {
        Map<String, Object> data = getErrorAttributes(request, ErrorAttributeOptions.of(ErrorAttributeOptions.Include.EXCEPTION,
                ErrorAttributeOptions.Include.BINDING_ERRORS,
                ErrorAttributeOptions.Include.MESSAGE));
        HttpStatus status = HttpStatus.resolve((Integer) data.get("status"));
        if (status != null) {
            if (status.is5xxServerError()) {
                throw new ServerException((String) data.get("message"), null);
            } else if (status.is4xxClientError()) {
                WebUtil.sendJsonResponse(R.optFailWithData(Map.of("error", data.get("message"))), status);
            }
            return;
        }
        super.error(request);
    }
}
