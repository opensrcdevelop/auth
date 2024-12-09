package cn.opensrcdevelop.common.util;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class WebUtilTests {

    @Test
    void getIpRegionTest() {
        assertThat(WebUtil.getIpRegion("127.0.0.1")).isEqualTo("内网IP");
        assertThat(WebUtil.getIpRegion("172.22.12.123")).isEqualTo("内网IP");
        assertThat(WebUtil.getIpRegion("192.168.10.1")).isEqualTo("内网IP");
        assertThat(WebUtil.getIpRegion("220.248.12.158")).isEqualTo("上海-上海市-联通");
        assertThat(WebUtil.getIpRegion("112.0.143.36")).isEqualTo("江苏省-苏州市-移动");
        assertThat(WebUtil.getIpRegion("47.52.236.180")).isEqualTo("香港-阿里云");
        assertThat(WebUtil.getIpRegion("164.114.53.60")).isEqualTo("美国");
    }
}
