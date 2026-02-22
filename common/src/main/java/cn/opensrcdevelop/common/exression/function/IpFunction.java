package cn.opensrcdevelop.common.exression.function;

import cn.opensrcdevelop.common.exression.ICustomFunction;
import java.net.InetAddress;
import org.springframework.stereotype.Component;

@Component
public class IpFunction implements ICustomFunction {

    public boolean isIpInCidr(String ipAddress, String cidr) {
        try {
            // 1. 分割CIDR获取IP部分和前缀长度
            String[] parts = cidr.split("/", 2);
            if (parts.length != 2) {
                throw new IllegalArgumentException("Invalid CIDR format: " + cidr);
            }

            // 2. 解析CIDR中的网络地址和要检查的IP地址
            InetAddress cidrAddress = InetAddress.getByName(parts[0]);
            InetAddress targetAddress = InetAddress.getByName(ipAddress);
            int prefixLength = Integer.parseInt(parts[1]);

            // 3. 检查地址类型是否匹配
            if (cidrAddress.getClass() != targetAddress.getClass()) {
                return false;
            }

            // 4. 获取字节数组并进行比较
            byte[] cidrBytes = cidrAddress.getAddress();
            byte[] targetBytes = targetAddress.getAddress();

            // 5. 处理IPv4和IPv6的不同长度
            int totalBits = cidrBytes.length * 8;
            if (prefixLength < 0 || prefixLength > totalBits) {
                throw new IllegalArgumentException("Invalid prefix length: " + prefixLength);
            }

            // 6. 逐字节比较网络部分
            int fullBytes = prefixLength / 8;
            int remainingBits = prefixLength % 8;

            // 7. 检查完整字节部分
            for (int i = 0; i < fullBytes; i++) {
                if (cidrBytes[i] != targetBytes[i]) {
                    return false;
                }
            }

            // 8. 检查剩余比特位
            if (remainingBits > 0) {
                byte mask = (byte) (0xFF << (8 - remainingBits));
                return (cidrBytes[fullBytes] & mask) == (targetBytes[fullBytes] & mask);
            }

            return true;
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    public String getNamespace() {
        return "ip";
    }
}
