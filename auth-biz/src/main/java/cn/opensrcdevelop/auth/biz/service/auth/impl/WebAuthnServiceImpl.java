package cn.opensrcdevelop.auth.biz.service.auth.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.dto.auth.*;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterOptionsResponseDto.Rp;
import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mfa.MfaValidContext;
import cn.opensrcdevelop.auth.biz.repository.auth.WebAuthnCredentialRepository;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.webauthn4j.WebAuthnManager;
import com.webauthn4j.credential.CredentialRecord;
import com.webauthn4j.credential.CredentialRecordImpl;
import com.webauthn4j.data.*;
import com.webauthn4j.data.attestation.AttestationObject;
import com.webauthn4j.data.attestation.authenticator.AAGUID;
import com.webauthn4j.data.attestation.authenticator.AttestedCredentialData;
import com.webauthn4j.data.attestation.authenticator.AuthenticatorData;
import com.webauthn4j.data.attestation.authenticator.COSEKey;
import com.webauthn4j.data.attestation.statement.COSEAlgorithmIdentifier;
import com.webauthn4j.data.client.Origin;
import com.webauthn4j.data.client.challenge.DefaultChallenge;
import com.webauthn4j.server.ServerProperty;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.stream.Collectors;

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

    private static final String PUB_KEY = "public-key";
    private static final Integer ALG_ES256 = -7;
    private static final Integer ALG_RSA256 = -257;

    /**
     * 获取注册选项
     *
     * @param userId
     *            用户ID
     * @param request
     *            HTTP请求
     * @return 注册选项响应 DTO
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
        p1.setType(PUB_KEY);
        p1.setAlg(ALG_ES256);
        params.add(p1);
        WebAuthnRegisterOptionsResponseDto.PubKeyCredParams p2 = new WebAuthnRegisterOptionsResponseDto.PubKeyCredParams();
        p2.setType(PUB_KEY);
        p2.setAlg(ALG_RSA256);
        params.add(p2);
        dto.setPubKeyCredParams(params.toArray(new WebAuthnRegisterOptionsResponseDto.PubKeyCredParams[0]));
        dto.setTimeout(60000L);

        // 配置认证器选择
        WebAuthnRegisterOptionsResponseDto.AuthenticatorSelection authenticatorSelection = new WebAuthnRegisterOptionsResponseDto.AuthenticatorSelection();
        // 要求可发现凭证
        authenticatorSelection.setResidentKey("required");
        // 优先要求用户验证
        authenticatorSelection.setUserVerification(true);
        dto.setAuthenticatorSelection(authenticatorSelection);

        // 获取已存在的凭证，用于排除
        List<WebAuthnCredential> existingCredentials = webAuthnCredentialRepository.findAllByUserId(userId);
        dto.setExcludeCredentials(CommonUtil.stream(existingCredentials).map(WebAuthnCredential::getCredentialId)
                .toList().toArray(new String[0]));

        return dto;
    }

    /**
     * 完成注册
     *
     * @param userId
     *            用户ID
     * @param requestDto
     *            注册完成请求
     * @param request
     *            HTTP请求
     * @return 凭证响应 DTO
     */
    @Override
    @Transactional
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.USER, userOperation = UserOperationType.BIND_MFA, success = "绑定了 Passkey 设备")
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

            // 获取 attestationObject
            String attestationObject = requestDto.getResponse() != null
                    ? requestDto.getResponse().getAttestationObject()
                    : null;
            if (StringUtils.isBlank(attestationObject)) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 解析并验证 attestationObject
            AttestationValidationResult validationResult = validateAttestation(requestDto, sessionChallenge, request);
            if (!validationResult.isValid()) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 获取 transports
            String transports = requestDto.getTransports();

            // 从 transports 推断设备类型
            String deviceType = getDeviceTypeFromTransports(transports);

            // 保存凭证到数据库（使用验证过的公钥 JSON）
            WebAuthnCredential credential = new WebAuthnCredential();
            credential.setUserId(userId);
            credential.setCredentialId(credentialId);
            credential.setPublicKey(validationResult.getPublicKeyJson());
            credential.setCounter(0L);
            credential.setDeviceType(deviceType);
            credential.setTransports(transports);
            credential.setDeleted(false);

            webAuthnCredentialRepository.save(credential);

            // 清除 session 中的 challenge
            session.removeAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE);

            return convertToCredentialResponseDto(credential);
        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("WebAuthn 注册失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
        }
    }

    /**
     * 获取认证选项（登录 和 MFA）
     *
     * @param userId
     *            用户ID（未登录时为 null）
     * @param request
     *            HTTP请求
     * @return 认证选项响应
     */
    @Override
    public WebAuthnAuthenticateOptionsResponseDto getAuthenticationOptions(String userId, HttpServletRequest request) {
        // 生成 challenge
        byte[] challengeBytes = new byte[32];
        new SecureRandom().nextBytes(challengeBytes);
        String challenge = Base64.getUrlEncoder().encodeToString(challengeBytes);

        // 保存 challenge 到 session
        request.getSession(true).setAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE, challenge);

        // 构建认证选项 DTO
        WebAuthnAuthenticateOptionsResponseDto dto = new WebAuthnAuthenticateOptionsResponseDto();
        dto.setChallenge(challenge);
        dto.setRpId(null);
        dto.setTimeout(60000L);
        dto.setUserVerification("preferred");

        if (StringUtils.isNotBlank(userId)) {
            List<WebAuthnCredential> credentials = webAuthnCredentialRepository.findAllByUserId(userId);
            if (!credentials.isEmpty()) {
                List<WebAuthnAuthenticateOptionsResponseDto.AllowCredential> allowCredentials = new ArrayList<>();
                for (WebAuthnCredential cred : credentials) {
                    WebAuthnAuthenticateOptionsResponseDto.AllowCredential ac = new WebAuthnAuthenticateOptionsResponseDto.AllowCredential();
                    ac.setId(cred.getCredentialId());
                    ac.setType(PUB_KEY);
                    if (StringUtils.isNotBlank(cred.getTransports())) {
                        ac.setTransports(cred.getTransports().split(CommonConstants.COMMA));
                    }
                    allowCredentials.add(ac);
                }
                dto.setAllowCredentials(allowCredentials);
                return dto;
            }
        }

        // 未登录或用户无凭证时，返回空 allowCredentials（浏览器会显示所有可用的 Passkey）
        dto.setAllowCredentials(new ArrayList<>());
        return dto;
    }

    /**
     * 完成认证（支持已登录和未登录场景）
     *
     * @param userId
     *            用户ID（未登录时为 null）
     * @param requestDto
     *            认证完成请求
     * @param request
     *            HTTP请求
     * @return 认证成功后的用户信息
     */
    @Override
    @Transactional
    public User completeAuthentication(String userId, WebAuthnAuthenticateCompleteRequestDto requestDto,
            HttpServletRequest request) {
        try {
            // 验证 session 中的 challenge
            HttpSession session = request.getSession(false);
            if (session == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1005);
            }
            String sessionChallenge = (String) session.getAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE);
            if (sessionChallenge == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1006);
            }

            // 查找凭证
            WebAuthnCredential credential = webAuthnCredentialRepository.findByCredentialId(requestDto.getId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleted())) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1004);
            }

            // 如果已登录，验证凭证属于该用户
            if (StringUtils.isNotBlank(userId) && !credential.getUserId().equals(userId)) {
                log.warn("凭证不属于该用户: expected={}, actual={}", userId, credential.getUserId());
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1004);
            }

            // 获取用户信息
            String credentialUserId = credential.getUserId();
            User user = userService.getById(credentialUserId);
            if (user == null) {
                throw new UsernameNotFoundException("account(" + credentialUserId + ") does not exist");
            }

            // 判断用户是否被禁用
            if (Boolean.TRUE.equals(user.getLocked())) {
                throw new BizException(MessageConstants.LOGIN_MSG_1003);
            }

            // 解码认证数据
            byte[] authenticatorDataBytes = Base64.getUrlDecoder().decode(requestDto.getResponse());
            byte[] clientDataJSONBytes = Base64.getUrlDecoder().decode(requestDto.getClientDataJSON());
            byte[] signatureBytes = Base64.getUrlDecoder().decode(requestDto.getSignature());

            if (authenticatorDataBytes == null || clientDataJSONBytes == null || signatureBytes == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 构建 ServerProperty
            ServerProperty serverProperty = ServerProperty.builder()
                    .origin(Origin.create(TenantHelper.getTenantConsoleUrl()))
                    .rpId(getRpId(request))
                    .challenge(new DefaultChallenge(sessionChallenge))
                    .build();

            // 从 JSON 公钥字符串创建 COSEKey
            COSEKey coseKey = objectMapper.readValue(credential.getPublicKey(), COSEKey.class);
            AttestedCredentialData attestedCredentialData = new AttestedCredentialData(
                    AAGUID.ZERO, credential.getCredentialId().getBytes(), coseKey);
            CredentialRecord credentialRecord = new CredentialRecordImpl(
                    null,
                    null,
                    null,
                    null,
                    credential.getCounter(),
                    attestedCredentialData,
                    null,
                    null,
                    null,
                    null);

            // 构建认证请求
            byte[] credentialIdBytes = Base64.getUrlDecoder().decode(requestDto.getId());
            List<byte[]> allowCredentials = new ArrayList<>();
            allowCredentials.add(credentialIdBytes);
            AuthenticationParameters parameters = new AuthenticationParameters(
                    serverProperty, credentialRecord, allowCredentials, true, true);
            AuthenticationRequest authenticationRequest = new AuthenticationRequest(
                    credentialIdBytes, null, authenticatorDataBytes, clientDataJSONBytes, null, signatureBytes);

            // 使用 webAuthnManager 验证
            WebAuthnManager webAuthnManager = WebAuthnManager.createNonStrictWebAuthnManager();
            webAuthnManager.verify(authenticationRequest, parameters);

            // 提取计数器并更新
            long presentedCounter = extractCounter(authenticatorDataBytes);
            if (presentedCounter > 0 && presentedCounter <= credential.getCounter()) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }
            credential.setCounter(presentedCounter);
            credential.setLastUsedAt(LocalDateTime.now());
            webAuthnCredentialRepository.update(credential);

            // 清除 session 中的 challenge
            session.removeAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE);

            // 设置验证状态
            setValid(credentialUserId, request);

            log.info("WebAuthn 认证成功: userId={}, credentialId={}", credentialUserId, requestDto.getId());
            return user;
        } catch (Exception e) {
            log.error("WebAuthn 认证失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001, e.getMessage());
        }
    }

    /**
     * Attestation 验证结果
     */
    @Data
    private static class AttestationValidationResult {
        private boolean valid;
        private String publicKeyJson;
    }

    /**
     * 验证 attestationObject
     *
     * @param requestDto
     *            注册请求 DTO
     * @param expectedChallenge
     *            期望的 challenge
     * @param request
     *            HTTP请求
     * @return 验证结果
     */
    private AttestationValidationResult validateAttestation(WebAuthnRegisterCompleteRequestDto requestDto,
            String expectedChallenge, HttpServletRequest request) {
        AttestationValidationResult result = new AttestationValidationResult();
        result.setValid(false);

        try {
            // 构建 ServerProperty
            ServerProperty serverProperty = ServerProperty.builder()
                    .origin(Origin.create(TenantHelper.getTenantConsoleUrl()))
                    .rpId(getRpId(request))
                    .challenge(new DefaultChallenge(expectedChallenge))
                    .build();

            // 构建 pubKeyCredParams
            List<PublicKeyCredentialParameters> pubKeyCredParams = Arrays.asList(
                    new PublicKeyCredentialParameters(PublicKeyCredentialType.PUBLIC_KEY,
                            COSEAlgorithmIdentifier.ES256),
                    new PublicKeyCredentialParameters(PublicKeyCredentialType.PUBLIC_KEY,
                            COSEAlgorithmIdentifier.RS256));

            // 构建 RegistrationParameters
            RegistrationParameters parameters = new RegistrationParameters(serverProperty, pubKeyCredParams, true,
                    true);

            // 使用 ObjectMapper 序列化请求对象为 JSON
            String registrationResponseJson = objectMapper.writeValueAsString(requestDto);

            // 使用 verifyRegistrationResponseJSON 解析和验证
            WebAuthnManager webAuthnManager = WebAuthnManager.createNonStrictWebAuthnManager();
            RegistrationData registrationData = webAuthnManager.verifyRegistrationResponseJSON(registrationResponseJson,
                    parameters);

            // 验证成功后，从 attestationObject 提取公钥
            AttestationObject attestationObj = registrationData.getAttestationObject();
            if (attestationObj != null) {
                AuthenticatorData<?> authData = attestationObj.getAuthenticatorData();
                AttestedCredentialData attestedCredentialData = authData.getAttestedCredentialData();
                if (attestedCredentialData != null) {
                    COSEKey coseKey = attestedCredentialData.getCOSEKey();
                    String publicKeyJson = objectMapper.writeValueAsString(coseKey);
                    result.setPublicKeyJson(publicKeyJson);
                    result.setValid(true);
                    log.info("WebAuthn 注册验证成功");
                } else {
                    log.error("无法从 attestationObject 中提取公钥");
                }
            }

            return result;
        } catch (Exception e) {
            log.error("验证 attestationObject 失败: {}", e.getMessage(), e);
            return result;
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
        // WebAuthn authenticatorData 结构:
        // - 32字节: RP ID Hash
        // - 1字节: flags (UP, UV, AT, ED 等)
        // - 4字节: signCount (如果 UP=1)
        // - 可选: credentialId length + credentialId (如果 AT=1)
        // - 可选: 扩展数据 (如果 ED=1)
        // 计数器位置取决于 flags 的值

        if (authenticatorData == null || authenticatorData.length < 37) {
            return 0;
        }

        int flags = authenticatorData[32] & 0xFF;
        // 如果 UP (User Present) = 0，则没有计数器
        if ((flags & 0x01) == 0) {
            return 0;
        }

        // 计数器在第 33-36 字节 (32 + 1 = 33，0-based index)
        // 标准 WebAuthn 认证中，计数器固定 4 字节
        return ((long) (authenticatorData[33] & 0xFF) << 24) |
                ((authenticatorData[34] & 0xFF) << 16) |
                ((authenticatorData[35] & 0xFF) << 8) |
                (authenticatorData[36] & 0xFF);
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
     *
     * 统计用户的凭证数量
     *
     * @param userId
     *            用户ID
     * @return 凭证数量
     */
    @Override
    public long countCredentials(String userId) {
        return webAuthnCredentialRepository.countByUserId(userId);
    }

    /**
     * 获取 RP ID（从请求的 Host 头提取域名作为 RP ID）
     *
     * @param request
     *            HTTP请求
     * @return RP ID（域名）
     */
    private String getRpId(HttpServletRequest request) {
        // 优先从请求头获取域名
        String host = WebUtil.getRootUrl();
        if (StringUtils.isNotBlank(host)) {
            // 去除端口号
            int portIndex = host.lastIndexOf(':');
            if (portIndex > 0) {
                host = host.substring(0, portIndex);
            }

            // 去除 schema
            int schemeIndex = host.indexOf("//");
            if (schemeIndex > 0) {
                host = host.substring(schemeIndex + 2);
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
     * 从 transports 中推断设备类型
     *
     * @param transports
     *            transports 字符串
     * @return 设备类型
     */
    private String getDeviceTypeFromTransports(String transports) {
        if (transports == null || transports.isEmpty()) {
            return "unknown";
        }
        String[] transportList = transports.split(",");
        boolean hasInternal = false;
        boolean hasExternal = false;
        for (String transport : transportList) {
            String t = transport.trim().toLowerCase();
            if ("internal".equals(t)) {
                hasInternal = true;
            } else if ("usb".equals(t) || "nfc".equals(t) || "hybrid".equals(t)) {
                hasExternal = true;
            }
        }
        if (hasInternal && !hasExternal) {
            return "platform"; // 平台认证器（Touch ID, Windows Hello）
        } else if (hasExternal && !hasInternal) {
            return "cross-platform"; // 跨平台认证器（USB Key, NFC）
        } else if (hasInternal) {
            return "cross-platform"; // 混合型，归类为跨平台
        }
        return "unknown";
    }

    /**
     * 转换为凭证响应 DTO
     *
     * @param credential
     *            WebAuthn 凭证
     * @return 凭证响应 DTO
     */
    private WebAuthnCredentialResponseDto convertToCredentialResponseDto(WebAuthnCredential credential) {
        WebAuthnCredentialResponseDto dto = new WebAuthnCredentialResponseDto();

        String id = credential.getCredentialId();
        dto.setId(id);
        dto.setDeviceType(credential.getDeviceType());
        dto.setCreatedAt(credential.getCreateTime());
        dto.setLastUsedAt(credential.getLastUsedAt());
        return dto;
    }

    /**
     * 设置 WebAuthn 认证成功
     *
     * @param userId
     *            用户ID
     * @param request
     *            HTTP请求
     */
    private void setValid(String userId, HttpServletRequest request) {
        HttpSession session = request.getSession(true);
        MfaValidContext context = new MfaValidContext();
        context.setValid(true);
        context.setUserId(userId);
        context.addValidatedMethod(AuthConstants.MFA_METHOD_WEBAUTHN);
        session.setAttribute(AuthConstants.MFA_VALID_CONTEXT, context);
    }
}
