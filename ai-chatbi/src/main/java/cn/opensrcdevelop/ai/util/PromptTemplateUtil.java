package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.common.exception.ServerException;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import org.apache.commons.collections4.MapUtils;

public class PromptTemplateUtil {

    private PromptTemplateUtil() {
    }

    /**
     * 获取 Prompt
     *
     * @param name
     *            模板名称
     * @param promptTemplate
     *            Prompt 模板
     * @param params
     *            参数
     * @return Prompt
     */
    public static String getPrompt(String name, String promptTemplate, Map<String, Object> params) {
        if (MapUtils.isEmpty(params)) {
            return promptTemplate;
        }

        try (StringReader reader = new StringReader(promptTemplate);
                StringWriter writer = new StringWriter()) {
            Template processor = new Template(name, reader, null, StandardCharsets.UTF_8.name());
            processor.process(params, writer);
            return writer.toString();
        } catch (IOException | TemplateException e) {
            throw new ServerException(e);
        }
    }
}
