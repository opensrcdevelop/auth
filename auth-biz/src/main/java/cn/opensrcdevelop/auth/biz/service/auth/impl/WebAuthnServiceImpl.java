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
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.webauthn4j.WebAuthnManager;
import com.webauthn4j.authenticator.AuthenticatorImpl;
import com.webauthn4j.converter.AttestationObjectConverter;
import com.webauthn4j.converter.util.ObjectConverter;
import com.webauthn4j.data.AuthenticationParameters;
import com.webauthn4j.data.AuthenticationRequest;
import com.webauthn4j.data.PublicKeyCredentialParameters;
import com.webauthn4j.data.PublicKeyCredentialType;
import com.webauthn4j.data.RegistrationData;
import com.webauthn4j.data.RegistrationParameters;
import com.webauthn4j.data.attestation.AttestationObject;
import com.webauthn4j.data.attestation.authenticator.AAGUID;
import com.webauthn4j.data.attestation.authenticator.AttestedCredentialData;
import com.webauthn4j.data.attestation.authenticator.AuthenticatorData;
import com.webauthn4j.data.attestation.authenticator.COSEKey;
import com.webauthn4j.data.attestation.statement.COSEAlgorithmIdentifier;
import com.webauthn4j.data.client.Origin;
import com.webauthn4j.data.client.challenge.Challenge;
import com.webauthn4j.data.client.challenge.DefaultChallenge;
import com.webauthn4j.server.ServerProperty;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Data;
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

        // 启用可发现凭证（resident keys），同一设备会返回相同 credentialId
        dto.setTimeout(60000L);

        // 配置认证器选择
        WebAuthnRegisterOptionsResponseDto.AuthenticatorSelection authenticatorSelection = new WebAuthnRegisterOptionsResponseDto.AuthenticatorSelection();
        authenticatorSelection.setResidentKey("required"); // 要求可发现凭证
        authenticatorSelection.setUserVerification(true); // 优先要求用户验证
        dto.setAuthenticatorSelection(authenticatorSelection);

        // 获取已存在的凭证，用于排除
        List<WebAuthnCredential> existingCredentials = webAuthnCredentialRepository.findAllByUserId(userId);
        if (!existingCredentials.isEmpty()) {
            List<String> excludeList = new ArrayList<>();
            for (WebAuthnCredential cred : existingCredentials) {
                if (Boolean.FALSE.equals(cred.getDeleted())) {
                    excludeList.add(cred.getCredentialId());
                }
            }
            dto.setExcludeCredentials(excludeList.toArray(new String[0]));
        } else {
            dto.setExcludeCredentials(new String[0]);
        }

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
                log.error("Session 为空，无法获取 challenge");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1005);
            }
            String sessionChallenge = (String) session.getAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE);
            log.info("Session ID: {}, Session 中的 challenge: {}", session.getId(), sessionChallenge);
            if (sessionChallenge == null) {
                log.error("Session 中未找到 challenge，可能已过期");
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
            log.info("收到 attestationObject: {}, 长度: {}",
                    attestationObject != null
                            ? attestationObject.substring(0, Math.min(50, attestationObject.length())) + "..."
                            : "null",
                    attestationObject != null ? attestationObject.length() : 0);
            if (StringUtils.isBlank(attestationObject)) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 获取 clientDataJSON
            String clientDataJSON = requestDto.getResponse() != null
                    ? requestDto.getResponse().getClientDataJSON()
                    : null;
            log.info("收到 clientDataJSON: {}, 长度: {}",
                    clientDataJSON != null
                            ? clientDataJSON.substring(0, Math.min(30, clientDataJSON.length())) + "..."
                            : "null",
                    clientDataJSON != null ? clientDataJSON.length() : 0);

            // 解析并验证 attestationObject（包含 challenge 验证和签名验证）
            AttestationValidationResult validationResult = validateAttestation(requestDto, sessionChallenge, request);
            if (!validationResult.isValid()) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 获取 transports
            String transports = requestDto.getTransports();
            if (StringUtils.isBlank(transports)) {
                transports = validationResult.getTransports();
            }

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
     * Attestation 验证结果
     */
    @Data
    private static class AttestationValidationResult {
        private boolean valid;
        private String publicKeyJson;
        private String transports;
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
        result.setTransports("internal,hybrid,usb,nfc");

        log.info("validateAttestation 开始 - expectedChallenge: {}", expectedChallenge);

        try {
            String originStr = TenantHelper.getTenantConsoleUrl();
            log.info("originStr: {}", originStr);
            Origin origin = Origin.create(originStr);
            Challenge challenge = new DefaultChallenge(expectedChallenge);
            ServerProperty serverProperty = new ServerProperty(origin, getRpId(request), challenge, null);

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
            AuthenticatorData<?> authData = attestationObj.getAuthenticatorData();
            AttestedCredentialData attestedCredentialData = authData.getAttestedCredentialData();

            if (attestedCredentialData != null && attestedCredentialData.getCOSEKey() != null) {
                COSEKey coseKey = attestedCredentialData.getCOSEKey();
                // 使用 Jackson 序列化 COSEKey 为 JSON 字符串
                String publicKeyJson = objectMapper.writeValueAsString(coseKey);
                result.setPublicKeyJson(publicKeyJson);
                result.setValid(true);
                log.info("WebAuthn 注册验证成功");
            } else {
                log.error("无法从 attestationObject 中提取公钥");
            }

            return result;
        } catch (Exception e) {
            log.error("验证 attestationObject 失败: {}", e.getMessage(), e);
            return result;
        }
    }

    /**
     * 获取认证选项（支持已登录和未登录场景）
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
        dto.setRpId(null); // 前端使用当前域名
        dto.setTimeout(60000L);
        dto.setUserVerification("preferred");

        // 如果用户已登录，填充凭证列表；否则返回空列表（浏览器会显示所有可用的 Passkey）
        if (StringUtils.isNotBlank(userId)) {
            List<WebAuthnCredential> credentials = webAuthnCredentialRepository.findAllByUserId(userId);
            if (!credentials.isEmpty()) {
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
        }

        // 未登录或用户无凭证时，返回空 allowCredentials（浏览器会显示所有可用的 Passkey）
        dto.setAllowCredentials(new ArrayList<>());
        return dto;
    }

    /**
     * 完成认证（支持已登录和未登录场景）
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
                throw new BizException(MessageConstants.USER_MSG_1003);
            }
            // 判断用户是否被禁用
            if (Boolean.TRUE.equals(user.getLocked())) {
                throw new BizException(MessageConstants.LOGIN_MSG_1003);
            }

            // 解码认证数据
            byte[] authenticatorDataBytes = base64UrlDecode(requestDto.getResponse());
            byte[] clientDataJSONBytes = base64UrlDecode(requestDto.getClientDataJSON());
            byte[] signatureBytes = base64UrlDecode(requestDto.getSignature());

            if (authenticatorDataBytes == null || clientDataJSONBytes == null || signatureBytes == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 调试日志
            log.info("authenticatorDataBytes length: {}", authenticatorDataBytes.length);
            log.info("response input length: {}",
                    requestDto.getResponse() != null ? requestDto.getResponse().length() : 0);

            // 构建 ServerProperty（使用控制台 URL 作为 origin）
            String originStr = TenantHelper.getTenantConsoleUrl();
            Origin origin = Origin.create(originStr);
            Challenge challenge = new DefaultChallenge(sessionChallenge);
            ServerProperty serverProperty = new ServerProperty(origin, getRpId(request), challenge, null);

            // 从 JSON 公钥字符串创建 COSEKey
            COSEKey coseKey = parseCOSEKey(credential.getPublicKey());

            // 构建 AuthenticatorImpl（作为凭证记录）
            AttestedCredentialData attestedCredentialData = new AttestedCredentialData(
                    AAGUID.ZERO, credential.getCredentialId().getBytes(), coseKey);
            AuthenticatorImpl authenticator = new AuthenticatorImpl(attestedCredentialData, null,
                    credential.getCounter(), null);

            // 构建认证请求（先解码 credentialId）
            byte[] credentialIdBytes = base64UrlDecode(requestDto.getId());

            // 构建认证参数（allowCredentials 格式必须与 authenticationRequest.credentialId 一致）
            List<byte[]> allowCredentials = new ArrayList<>();
            allowCredentials.add(credentialIdBytes);
            AuthenticationParameters parameters = new AuthenticationParameters(
                    serverProperty, authenticator, allowCredentials, true, true);

            AuthenticationRequest authenticationRequest = new AuthenticationRequest(
                    credentialIdBytes, null, authenticatorDataBytes, clientDataJSONBytes, null, signatureBytes);

            // 使用 webAuthnManager 验证
            WebAuthnManager webAuthnManager = WebAuthnManager.createNonStrictWebAuthnManager();
            webAuthnManager.validate(authenticationRequest, parameters);

            // 提取计数器并更新
            long presentedCounter = extractCounter(authenticatorDataBytes);
            log.info("计数器: presented={}, stored={}", presentedCounter, credential.getCounter());
            // 首次认证时 presentedCounter=0，stored=0 是正常的
            // 后续认证要求 presentedCounter > stored
            if (presentedCounter == 0 && credential.getCounter() == 0) {
                // 首次认证，跳过计数器递增验证
                log.info("首次认证，跳过计数器验证");
            } else if (presentedCounter <= credential.getCounter()) {
                log.warn("计数器验证失败: presented={}, stored={}", presentedCounter, credential.getCounter());
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }
            credential.setCounter(presentedCounter);
            credential.setLastUsedAt(LocalDateTime.now());
            webAuthnCredentialRepository.update(credential);
            log.info("计数器已更新: credentialId={}, newCounter={}", requestDto.getId(), presentedCounter);

            // 清除 session 中的 challenge
            session.removeAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE);

            // 设置验证状态
            setValid(credentialUserId, request);

            log.info("WebAuthn 认证成功: userId={}, credentialId={}", credentialUserId, requestDto.getId());
            return user;
        } catch (BizException e) {
            log.warn("WebAuthn 认证业务异常: {}", e.getMessage());
            throw e;
        } catch (Exception e) {
            log.error("WebAuthn 认证失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001, e.getMessage());
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
        if (authenticatorData.length < 37) {
            return 0;
        }

        long counter = ((authenticatorData[33] & 0xFF) << 24) |
                ((authenticatorData[34] & 0xFF) << 16) |
                ((authenticatorData[35] & 0xFF) << 8) |
                (authenticatorData[36] & 0xFF);
        return counter;
    }

    /**
     * 从 JSON 字符串解析 COSE Key
     */
    private COSEKey parseCOSEKey(String publicKeyJson) {
        if (StringUtils.isBlank(publicKeyJson)) {
            log.error("公钥 JSON 为空");
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001, "公钥数据为 null");
        }

        try {
            // 使用 Jackson 反序列化为 COSEKey
            COSEKey coseKey = objectMapper.readValue(publicKeyJson, COSEKey.class);
            log.info("从 JSON 成功解析 COSE Key: kty={}", coseKey.getKeyType());
            return coseKey;
        } catch (Exception e) {
            log.error("解析 COSE Key 失败: {}", e.getMessage(), e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001, "公钥解析失败");
        }
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
        } else if (hasInternal && hasExternal) {
            return "cross-platform"; // 混合型，归类为跨平台
        }
        return "unknown";
    }

    /**
     * 从 attestationObject 中提取公钥（JSON格式）
     *
     * @param attestationObjectBase64
     *            Base64URL 编码的 attestationObject
     * @return JSON 格式的公钥
     */
    private String extractPublicKeyFromAttestation(String attestationObjectBase64) {
        try {
            // 解码 attestationObject
            byte[] attestationObjectBytes = base64UrlDecode(attestationObjectBase64);
            if (attestationObjectBytes == null || attestationObjectBytes.length == 0) {
                log.error("attestationObject 为空");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 使用 AttestationObjectConverter 解析
            AttestationObjectConverter attestationObjectConverter = new AttestationObjectConverter(
                    new ObjectConverter());
            AttestationObject attestationObject = attestationObjectConverter
                    .convert(attestationObjectBytes);

            // 获取公钥
            AuthenticatorData<?> authenticatorData = attestationObject.getAuthenticatorData();
            if (authenticatorData == null) {
                log.error("attestationObject 中未找到 authenticatorData");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            AttestedCredentialData attestedCredentialData = authenticatorData.getAttestedCredentialData();
            if (attestedCredentialData == null) {
                log.error("attestationObject 中未找到 attestedCredentialData");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            COSEKey coseKey = attestedCredentialData.getCOSEKey();
            if (coseKey == null) {
                log.error("attestationObject 中未找到公钥");
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 使用 Jackson 序列化 COSEKey 为 JSON 字符串
            String publicKeyJson = objectMapper.writeValueAsString(coseKey);
            log.info("成功提取公钥 JSON，长度: {} 字符", publicKeyJson.length());
            return publicKeyJson;

        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("解析 attestationObject 失败: {}", e.getMessage());
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
        }
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
        StringBuilder sb = new StringBuilder(base64);
        for (int i = 0; i < padding; i++) {
            sb.append('=');
        }
        return Base64.getDecoder().decode(sb.toString());
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
