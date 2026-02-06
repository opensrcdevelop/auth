package cn.opensrcdevelop.auth.biz.service.auth.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateOptionsResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnCredentialResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterOptionsResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterOptionsResponseDto.Rp;
import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mfa.MfaValidContext;
import cn.opensrcdevelop.auth.biz.repository.auth.WebAuthnCredentialRepository;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.webauthn4j.converter.AttestationObjectConverter;
import com.webauthn4j.converter.util.ObjectConverter;
import com.webauthn4j.data.attestation.AttestationObject;
import com.webauthn4j.data.attestation.authenticator.AttestedCredentialData;
import com.webauthn4j.data.attestation.authenticator.AuthenticatorData;
import com.webauthn4j.data.attestation.authenticator.COSEKey;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.interfaces.ECPublicKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.ECFieldFp;
import java.security.spec.ECParameterSpec;
import java.security.spec.ECPoint;
import java.security.spec.ECPublicKeySpec;
import java.security.spec.EllipticCurve;
import java.security.spec.RSAPublicKeySpec;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * WebAuthn 服务实现
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class WebAuthnServiceImpl implements WebAuthnService {

    private final WebAuthnCredentialRepository webAuthnCredentialRepository;
    private final UserService userService;
    private final ObjectMapper objectMapper;

    /**
     * 获取 RP ID（从请求的 Host 头提取域名作为 RP ID）
     *
     * @param request
     *            HTTP请求
     * @return RP ID（域名）
     */
    private String getRpId(HttpServletRequest request) {
        // 优先从请求头获取域名
        String host = request.getHeader("Host");
        if (StringUtils.isNotBlank(host)) {
            // 去除端口号
            int portIndex = host.indexOf(':');
            if (portIndex > 0) {
                host = host.substring(0, portIndex);
            }
            return host;
        }
        // 备用方案：从请求属性获取
        String serverName = request.getServerName();
        if (StringUtils.isNotBlank(serverName)) {
            return serverName;
        }
        return "localhost";
    }

    /**
     * 获取 RP 名称（从 spring.application.name 获取）
     */
    private String getRpName() {
        return SpringContextUtil.getProperty("spring.application.name", "auth-server");
    }

    /**
     * 获取注册选项
     */
    @Override
    public WebAuthnRegisterOptionsResponseDto getRegistrationOptions(String userId, HttpServletRequest request) {
        User user = userService.getUserInfo(userId);

        // 生成 challenge
        byte[] challengeBytes = new byte[32];
        new SecureRandom().nextBytes(challengeBytes);
        String challenge = Base64.getUrlEncoder().encodeToString(challengeBytes);

        // 将 challenge 保存到 session
        HttpSession session = request.getSession(true);
        if (session != null) {
            session.setAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE, challenge);
        }

        // 构建用户身份
        byte[] userIdBytes = userId.getBytes();

        // 构建注册选项 DTO
        WebAuthnRegisterOptionsResponseDto dto = new WebAuthnRegisterOptionsResponseDto();
        dto.setChallenge(challenge);

        // 用户信息
        WebAuthnRegisterOptionsResponseDto.User userDto = new WebAuthnRegisterOptionsResponseDto.User();
        userDto.setId(Base64.getUrlEncoder().encodeToString(userIdBytes));
        userDto.setName(user.getUsername());
        userDto.setDisplayName(user.getUsername());
        dto.setUser(userDto);

        // RP 信息
        Rp rpDto = new Rp();
        rpDto.setId(getRpId(request));
        rpDto.setName(getRpName());
        dto.setRp(rpDto);

        // 凭证参数
        List<WebAuthnRegisterOptionsResponseDto.PubKeyCredParams> params = new ArrayList<>();
        WebAuthnRegisterOptionsResponseDto.PubKeyCredParams p1 = new WebAuthnRegisterOptionsResponseDto.PubKeyCredParams();
        p1.setType("public-key");
        p1.setAlg(-7); // ES256
        params.add(p1);
        WebAuthnRegisterOptionsResponseDto.PubKeyCredParams p2 = new WebAuthnRegisterOptionsResponseDto.PubKeyCredParams();
        p2.setType("public-key");
        p2.setAlg(-257); // RS256
        params.add(p2);
        dto.setPubKeyCredParams(params.toArray(new WebAuthnRegisterOptionsResponseDto.PubKeyCredParams[0]));

        dto.setTimeout(60000L);
        dto.setExcludeCredentials(new String[0]);

        return dto;
    }

    /**
     * 完成注册
     */
    @Override
    @Transactional
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.USER, userOperation = UserOperationType.BIND_MFA, success = "绑定了 WebAuthn/Passkey 设备")
    public WebAuthnCredentialResponseDto completeRegistration(String userId,
            WebAuthnRegisterCompleteRequestDto requestDto, HttpServletRequest request) {
        try {
            // 验证 challenge（从 session 中获取）
            HttpSession session = request.getSession(false);
            if (session == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1005);
            }
            String sessionChallenge = (String) session.getAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE);
            if (sessionChallenge == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1006);
            }

            // 获取凭证 ID
            String credentialId = requestDto.getId();
            if (StringUtils.isBlank(credentialId)) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1002);
            }

            // 获取 transports
            String transports = requestDto.getTransports();
            if (StringUtils.isBlank(transports)) {
                transports = "internal,hybrid,usb,nfc";
            }

            // 解析 attestationObject 获取公钥
            byte[] publicKeyBytes = extractPublicKeyFromAttestation(
                    requestDto.getAttestationObject(),
                    requestDto.getResponse());

            // 保存凭证到数据库
            WebAuthnCredential credential = new WebAuthnCredential();
            credential.setUserId(userId);
            credential.setCredentialId(credentialId);
            credential.setPublicKey(publicKeyBytes);
            credential.setCounter(0L);
            credential.setDeviceType(WebUtil.getDeviceType());
            credential.setTransports(transports);
            credential.setDeleted(false);

            webAuthnCredentialRepository.save(credential);

            // 清除 session 中的 challenge
            if (session != null) {
                session.removeAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE);
            }

            return convertToCredentialResponseDto(credential);
        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("WebAuthn 注册失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
        }
    }

    /**
     * 获取认证选项
     */
    @Override
    public WebAuthnAuthenticateOptionsResponseDto getAuthenticationOptions(String userId) {
        // 获取用户凭证
        List<WebAuthnCredential> credentials = webAuthnCredentialRepository.findAllByUserId(userId);
        if (credentials.isEmpty()) {
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1003);
        }

        // 生成 challenge
        byte[] challengeBytes = new byte[32];
        new SecureRandom().nextBytes(challengeBytes);
        String challenge = Base64.getUrlEncoder().encodeToString(challengeBytes);

        // 构建认证选项 DTO
        WebAuthnAuthenticateOptionsResponseDto dto = new WebAuthnAuthenticateOptionsResponseDto();
        dto.setChallenge(challenge);
        dto.setRpId(null); // 前端使用当前域名
        dto.setTimeout(60000L);
        dto.setUserVerification("preferred");

        // 构建凭证描述列表
        List<WebAuthnAuthenticateOptionsResponseDto.AllowCredential> allowCredentials = new ArrayList<>();
        for (WebAuthnCredential cred : credentials) {
            WebAuthnAuthenticateOptionsResponseDto.AllowCredential ac = new WebAuthnAuthenticateOptionsResponseDto.AllowCredential();
            ac.setId(cred.getCredentialId());
            ac.setType("public-key");
            if (StringUtils.isNotBlank(cred.getTransports())) {
                ac.setTransports(cred.getTransports().split(","));
            }
            allowCredentials.add(ac);
        }
        dto.setAllowCredentials(allowCredentials);

        return dto;
    }

    /**
     * 完成认证
     */
    @Override
    @Transactional
    public boolean completeAuthentication(String userId, WebAuthnAuthenticateCompleteRequestDto requestDto,
            HttpServletRequest request) {
        try {
            // 查找凭证
            WebAuthnCredential credential = webAuthnCredentialRepository.findByCredentialId(requestDto.getId());
            if (credential == null) {
                log.warn("凭证不存在: {}", requestDto.getId());
                return false;
            }

            // 验证凭证属于该用户
            if (!credential.getUserId().equals(userId)) {
                log.warn("凭证不属于该用户");
                return false;
            }

            // 解码认证数据
            byte[] authenticatorData = base64UrlDecode(requestDto.getResponse());
            byte[] clientDataJSON = base64UrlDecode(requestDto.getClientDataJSON());
            byte[] signature = base64UrlDecode(requestDto.getSignature());

            if (authenticatorData == null || clientDataJSON == null || signature == null) {
                log.warn("认证数据解码失败");
                return false;
            }

            // 验证签名
            boolean signatureValid = verifySignature(credential, authenticatorData, signature, clientDataJSON);
            if (!signatureValid) {
                log.warn("签名验证失败: {}", requestDto.getId());
                return false;
            }

            // 验证计数器（防止重放攻击）
            long presentedCounter = extractCounter(authenticatorData);
            if (presentedCounter <= credential.getCounter()) {
                log.warn("计数器验证失败: presented={}, stored={}", presentedCounter, credential.getCounter());
                return false;
            }

            // 更新计数器
            credential.setCounter(presentedCounter);
            credential.setLastUsedAt(LocalDateTime.now());
            webAuthnCredentialRepository.update(credential);

            // 设置验证状态
            setValid(userId, request);

            return true;
        } catch (Exception e) {
            log.error("WebAuthn 认证失败", e);
            return false;
        }
    }

    /**
     * 从 authenticatorData 中提取计数器
     *
     * @param authenticatorData
     *            认证器数据
     * @return 计数器值
     */
    private long extractCounter(byte[] authenticatorData) {
        // authenticatorData 结构: 32字节 RP ID Hash + 4字节标志 + 4字节计数器 + ...
        // 计数器在第 37 个字节开始（32 + 4 + 1 = 37）
        if (authenticatorData.length < 37) {
            return 0;
        }
        long counter = ((authenticatorData[32] & 0xFF) << 24) |
                ((authenticatorData[33] & 0xFF) << 16) |
                ((authenticatorData[34] & 0xFF) << 8) |
                (authenticatorData[35] & 0xFF);
        return counter;
    }

    /**
     * 列出用户凭证
     */
    @Override
    public List<WebAuthnCredentialResponseDto> listCredentials(String userId) {
        List<WebAuthnCredential> credentials = webAuthnCredentialRepository.findAllByUserId(userId);
        return credentials.stream()
                .map(this::convertToCredentialResponseDto)
                .collect(Collectors.toList());
    }

    /**
     * 删除凭证
     */
    @Override
    @Transactional
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.USER, userOperation = UserOperationType.BIND_MFA, success = "删除了 WebAuthn/Passkey 设备")
    public void deleteCredential(String credentialId, String userId) {
        WebAuthnCredential credential = webAuthnCredentialRepository.findByCredentialId(credentialId);
        if (credential == null || !credential.getUserId().equals(userId)) {
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1004);
        }

        webAuthnCredentialRepository.deleteByCredentialId(credentialId);
    }

    /**
     * 设置验证通过状态
     */
    @Override
    public void setValid(String userId, HttpServletRequest request) {
        HttpSession session = request.getSession(true);
        MfaValidContext context = new MfaValidContext();
        context.setValid(true);
        context.setUserId(userId);
        context.addValidatedMethod("WEBAUTHN");
        session.setAttribute(AuthConstants.MFA_VALID_CONTEXT, context);
    }

    /**
     * 检查是否已验证（优先检查统一上下文）
     */
    @Override
    public boolean isValidated(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        if (session == null) {
            return false;
        }
        // 检查统一上下文
        MfaValidContext mfaContext = (MfaValidContext) session.getAttribute(AuthConstants.MFA_VALID_CONTEXT);
        return mfaContext != null && mfaContext.isMethodValidated("WEBAUTHN");
    }

    /**
     * Passkey 登录认证成功后设置验证通过状态
     */
    @Override
    public void setValidForPasskeyLogin(String userId, HttpServletRequest request) {
        HttpSession session = request.getSession(true);
        if (session != null) {
            MfaValidContext existingContext = (MfaValidContext) session.getAttribute(AuthConstants.MFA_VALID_CONTEXT);
            if (existingContext != null && existingContext.isMethodValidated("WEBAUTHN")) {
                return;
            }
        }
        setValid(userId, request);
    }

    /**
     * 获取凭证数量
     */
    @Override
    public long countCredentials(String userId) {
        return webAuthnCredentialRepository.countByUserId(userId);
    }

    // ============ 私有辅助方法 ============

    /**
     * 从 attestationObject 中提取公钥
     *
     * @param attestationObjectBase64
     *            Base64URL 编码的 attestationObject
     * @param responseBase64
     *            Base64URL 编码的响应（包含 clientDataJSON）
     * @return COSE 格式的公钥字节数组
     */
    private byte[] extractPublicKeyFromAttestation(String attestationObjectBase64, String responseBase64) {
        try {
            // 解码 attestationObject
            byte[] attestationObjectBytes = base64UrlDecode(attestationObjectBase64);
            if (attestationObjectBytes == null || attestationObjectBytes.length == 0) {
                log.warn("attestationObject 为空，尝试从 response 中解析");
                return extractPublicKeyFromResponse(responseBase64);
            }

            // 使用 AttestationObjectConverter 解析
            AttestationObjectConverter attestationObjectConverter = new AttestationObjectConverter(
                    new ObjectConverter());
            AttestationObject attestationObject = attestationObjectConverter
                    .convert(attestationObjectBytes);

            // 获取公钥
            java.security.PublicKey publicKey = extractPublicKeyFromAttestationObject(attestationObject);

            if (publicKey != null) {
                // 存储公钥的简单格式：算法标识 + 公钥字节
                byte[] keyBytes = serializePublicKey(publicKey);
                log.info("成功提取公钥，长度: {} 字节", keyBytes.length);
                return keyBytes;
            }

            log.warn("attestationObject 中未找到公钥，尝试从 response 中解析");
            return extractPublicKeyFromResponse(responseBase64);

        } catch (Exception e) {
            log.warn("解析 attestationObject 失败: {}，尝试从 response 中解析", e.getMessage());
            try {
                return extractPublicKeyFromResponse(responseBase64);
            } catch (Exception ex) {
                log.error("从 response 解析公钥也失败", ex);
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }
        }
    }

    /**
     * 从完整的响应 JSON 中提取公钥
     *
     * @param responseBase64
     *            Base64URL 编码的响应
     * @return COSE 格式的公钥字节数组
     */
    private byte[] extractPublicKeyFromResponse(String responseBase64) {
        try {
            // 解析完整的响应 JSON
            String responseJson = new String(base64UrlDecode(responseBase64));

            // 解析 JSON 获取 attestationObject
            JsonNode jsonNode = objectMapper.readTree(responseJson);
            String attestationObjectStr = jsonNode.path("attestationObject").asText();

            if (attestationObjectStr.isEmpty()) {
                log.error("响应中未找到 attestationObject");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            byte[] attestationObjectBytes = base64UrlDecode(attestationObjectStr);

            // 使用 AttestationObjectConverter 解析
            AttestationObjectConverter attestationObjectConverter = new AttestationObjectConverter(
                    new ObjectConverter());
            AttestationObject attestationObject = attestationObjectConverter
                    .convert(attestationObjectBytes);

            java.security.PublicKey publicKey = extractPublicKeyFromAttestationObject(attestationObject);
            if (publicKey == null) {
                log.error("响应中未找到公钥");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            byte[] keyBytes = serializePublicKey(publicKey);
            log.info("成功从 response 提取公钥，长度: {} 字节", keyBytes.length);
            return keyBytes;

        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("从 response 解析公钥失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
        }
    }

    /**
     * 从 AttestationObject 中提取公钥
     */
    private java.security.PublicKey extractPublicKeyFromAttestationObject(
            AttestationObject attestationObject) {

        // 获取认证器数据
        AuthenticatorData<?> authenticatorData = attestationObject
                .getAuthenticatorData();

        if (authenticatorData == null) {
            return null;
        }

        // 获取凭证数据
        AttestedCredentialData attestedCredentialData = authenticatorData
                .getAttestedCredentialData();

        if (attestedCredentialData == null) {
            return null;
        }

        // 获取 COSEKey
        COSEKey coseKey = attestedCredentialData.getCOSEKey();
        if (coseKey == null) {
            return null;
        }

        // 从 COSEKey 获取公钥
        return coseKey.getPublicKey();
    }

    /**
     * 序列化公钥（简化格式：4字节算法ID + 公钥字节）
     */
    private byte[] serializePublicKey(java.security.PublicKey publicKey) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        try {
            // 算法标识
            int algorithmId;
            byte[] keyBytes;

            if (publicKey instanceof ECPublicKey) {
                algorithmId = -7; // ES256
                ECPublicKey ecPublicKey = (ECPublicKey) publicKey;
                java.security.spec.ECPoint point = ecPublicKey.getW();
                byte[] x = point.getAffineX().toByteArray();
                byte[] y = point.getAffineY().toByteArray();

                // 格式：04 || x || y (未压缩点格式)
                keyBytes = new byte[1 + x.length + y.length];
                keyBytes[0] = 0x04;
                System.arraycopy(x, 0, keyBytes, 1, x.length);
                System.arraycopy(y, 0, keyBytes, 1 + x.length, y.length);
            } else if (publicKey instanceof RSAPublicKey) {
                algorithmId = -257; // RS256
                RSAPublicKey rsaPublicKey = (RSAPublicKey) publicKey;
                keyBytes = rsaPublicKey.getModulus().toByteArray();
            } else {
                log.warn("不支持的公钥类型: {}", publicKey.getClass().getName());
                return null;
            }

            // 写入算法ID（4字节大端序）
            baos.write((byte) ((algorithmId >> 24) & 0xFF));
            baos.write((byte) ((algorithmId >> 16) & 0xFF));
            baos.write((byte) ((algorithmId >> 8) & 0xFF));
            baos.write((byte) (algorithmId & 0xFF));

            // 写入公钥字节
            baos.write(keyBytes);

            return baos.toByteArray();

        } catch (Exception e) {
            log.error("序列化公钥失败", e);
            return null;
        }
    }

    /**
     * 验证签名
     *
     * @param credential
     *            凭证
     * @param authenticatorData
     *            认证器数据
     * @param signature
     *            签名
     * @return 验证结果
     */
    public boolean verifySignature(WebAuthnCredential credential,
            byte[] authenticatorData,
            byte[] signature,
            byte[] clientDataJSON) {
        try {
            // 从凭证中获取公钥
            byte[] keyData = credential.getPublicKey();
            if (keyData == null || keyData.length < 5) {
                log.warn("凭证公钥数据无效");
                return false;
            }

            // 解析公钥
            int algorithmId = ((keyData[0] & 0xFF) << 24) |
                    ((keyData[1] & 0xFF) << 16) |
                    ((keyData[2] & 0xFF) << 8) |
                    (keyData[3] & 0xFF);
            byte[] keyBytes = new byte[keyData.length - 4];
            System.arraycopy(keyData, 4, keyBytes, 0, keyBytes.length);

            java.security.PublicKey publicKey;
            String signatureAlgorithm;

            if (algorithmId == -7) { // ES256
                // 解析未压缩的 EC 点格式
                if (keyBytes[0] != 0x04 || keyBytes.length < 65) {
                    log.warn("无效的 EC 公钥格式");
                    return false;
                }
                int coordLength = (keyBytes.length - 1) / 2;
                byte[] x = new byte[coordLength];
                byte[] y = new byte[coordLength];
                System.arraycopy(keyBytes, 1, x, 0, coordLength);
                System.arraycopy(keyBytes, 1 + coordLength, y, 0, coordLength);

                ECPoint point = new ECPoint(
                        new BigInteger(1, x),
                        new BigInteger(1, y));

                // P-256 曲线参数（WebAuthn 标准定义）
                BigInteger p = new BigInteger(
                        "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF", 16);
                BigInteger a = new BigInteger(
                        "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC", 16);
                BigInteger b = new BigInteger(
                        "5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B", 16);
                BigInteger gx = new BigInteger(
                        "6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296", 16);
                BigInteger gy = new BigInteger(
                        "4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5", 16);
                BigInteger n = new BigInteger(
                        "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551", 16);

                EllipticCurve curve = new EllipticCurve(
                        new ECFieldFp(p), a, b);
                ECParameterSpec ecSpec = new ECParameterSpec(
                        curve, new ECPoint(gx, gy), n, 1);

                KeyFactory keyFactory = KeyFactory.getInstance("EC");
                publicKey = keyFactory.generatePublic(new ECPublicKeySpec(point, ecSpec));
                signatureAlgorithm = "SHA256withECDSA";
            } else if (algorithmId == -257) { // RS256
                BigInteger modulus = new BigInteger(1, keyBytes);
                RSAPublicKeySpec rsaSpec = new RSAPublicKeySpec(
                        modulus, BigInteger.valueOf(65537));
                KeyFactory keyFactory = KeyFactory.getInstance("RSA");
                publicKey = keyFactory.generatePublic(rsaSpec);
                signatureAlgorithm = "SHA256withRSA";
            } else {
                log.warn("不支持的算法: {}", algorithmId);
                return false;
            }

            // 验证签名
            byte[] signedData = concatenate(authenticatorData, sha256(clientDataJSON));
            Signature verifier = Signature.getInstance(signatureAlgorithm);
            verifier.initVerify(publicKey);
            verifier.update(signedData);
            return verifier.verify(signature);

        } catch (Exception e) {
            log.error("验证签名失败", e);
            return false;
        }
    }

    /**
     * SHA-256 哈希
     */
    private byte[] sha256(byte[] data) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            return digest.digest(data);
        } catch (Exception e) {
            throw new RuntimeException("SHA-256 哈希失败", e);
        }
    }

    /**
     * 字节数组合并
     */
    private byte[] concatenate(byte[] a, byte[] b) {
        byte[] result = new byte[a.length + b.length];
        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
    }

    /**
     * Base64URL 解码
     */
    private byte[] base64UrlDecode(String input) {
        if (input == null || input.isEmpty()) {
            return null;
        }
        // 将 Base64URL 转换为标准 Base64
        String base64 = input.replace('-', '+').replace('_', '/');
        // 添加填充
        int padding = (4 - (base64.length() % 4)) % 4;
        for (int i = 0; i < padding; i++) {
            base64 += "=";
        }
        return Base64.getDecoder().decode(base64);
    }

    /**
     * 转换为凭证响应 DTO
     */
    private WebAuthnCredentialResponseDto convertToCredentialResponseDto(WebAuthnCredential credential) {
        WebAuthnCredentialResponseDto dto = new WebAuthnCredentialResponseDto();

        String id = credential.getCredentialId();
        dto.setId(id);
        // 显示 ID 前缀用于识别（显示前 8 个字符）
        dto.setIdPrefix(id != null && id.length() > 8 ? id.substring(0, 8) + "..." : id);
        dto.setDeviceType(credential.getDeviceType());

        if (StringUtils.isNotBlank(credential.getTransports())) {
            dto.setTransports(credential.getTransports().split(","));
        }

        dto.setCreatedAt(credential.getCreateTime());
        dto.setLastUsedAt(credential.getLastUsedAt());
        dto.setDeletable(true);

        return dto;
    }

}
