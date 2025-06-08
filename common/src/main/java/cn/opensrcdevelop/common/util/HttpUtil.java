package cn.opensrcdevelop.common.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.lang.NonNull;
import org.springframework.util.StreamUtils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class HttpUtil {
    private HttpUtil() {
    }

    @Slf4j
    public static class CustomClientHttpRequestInterceptor implements ClientHttpRequestInterceptor {

        @Override
        @NonNull
        public ClientHttpResponse intercept(HttpRequest request, @NonNull byte[] bytes, @NonNull ClientHttpRequestExecution execution) throws IOException {
            log.info("HTTP Method: {}, URI: {}, Headers: {}", request.getMethod(), request.getURI(), request.getHeaders());
            request.getMethod();
            if (request.getMethod().equals(HttpMethod.POST)) {
                log.info("Request Body: {}", new String(bytes, StandardCharsets.UTF_8));
            }

            ClientHttpResponse response = execution.execute(request, bytes);
            ClientHttpResponse responseWrapper = new HttpUtil.BufferingClientHttpResponseWrapper(response);

            String body = StreamUtils.copyToString(responseWrapper.getBody(), StandardCharsets.UTF_8);
            log.info("Response Body: {}", body);

            return responseWrapper;
        }
    }

    static class BufferingClientHttpResponseWrapper implements ClientHttpResponse {

        private final ClientHttpResponse response;
        private byte[] body;

        BufferingClientHttpResponseWrapper(ClientHttpResponse response) {
            this.response = response;
        }

        @Override
        @NonNull
        public HttpStatusCode getStatusCode() throws IOException {
            return this.response.getStatusCode();
        }

        @Override
        @NonNull
        public String getStatusText() throws IOException {
            return this.response.getStatusText();
        }

        @Override
        public void close() {
            this.response.close();
        }

        @Override
        @NonNull
        public InputStream getBody() throws IOException {
            if (this.body == null) {
                this.body = StreamUtils.copyToByteArray(this.response.getBody());
            }
            return new ByteArrayInputStream(this.body);
        }

        @Override
        @NonNull
        public HttpHeaders getHeaders() {
            return this.response.getHeaders();
        }
    }
}
