package cn.opensrcdevelop.common.controller;

import cn.opensrcdevelop.common.annoation.NoPathPrefix;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.autoconfigure.web.ErrorProperties;
import org.springframework.boot.autoconfigure.web.servlet.error.BasicErrorController;
import org.springframework.boot.web.error.ErrorAttributeOptions;
import org.springframework.boot.web.servlet.error.DefaultErrorAttributes;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 处理过滤器抛出的异常
 */
@NoPathPrefix
@RestController
public class ExceptionController extends BasicErrorController {

    private static final String STATUS = "status";
    private static final String ERROR = "error";
    private static final String MESSAGE = "message";

    public ExceptionController() {
        super(new DefaultErrorAttributes(), new ErrorProperties());
    }

    @RequestMapping(produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.TEXT_HTML_VALUE})
    public void errorHandler(HttpServletRequest request) {
        Map<String, Object> data = getErrorAttributes(request, ErrorAttributeOptions.of(
                ErrorAttributeOptions.Include.STATUS,
                ErrorAttributeOptions.Include.ERROR,
                ErrorAttributeOptions.Include.EXCEPTION,
                ErrorAttributeOptions.Include.MESSAGE));
        HttpStatus status;
        if (data.get(STATUS) != null && (status = HttpStatus.resolve((Integer) data.get(STATUS))) != null) {
            if (status.is5xxServerError()) {
                throw new ServerException(data.get(MESSAGE) != null ? (String) data.get(MESSAGE) : StringUtils.EMPTY,
                        null);
            } else if (status.is4xxClientError()) {
                WebUtil.sendJsonResponse(R.optFailWithData(Map.of(ERROR, data.get(MESSAGE))), status);
            }
            return;
        }
        super.error(request);
    }
}
