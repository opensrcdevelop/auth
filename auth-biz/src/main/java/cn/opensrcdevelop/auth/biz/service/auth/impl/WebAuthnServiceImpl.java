package cn.opensrcdevelop.auth.biz.service.auth.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.component.authserver.UserTokenBasedRememberMeServices;
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
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class WebAuthnServiceImpl implements WebAuthnService {

    private final WebAuthnCredentialRepository webAuthnCredentialRepository;
    private final UserService userService;
    private final ObjectMapper objectMapper;
    private final UserTokenBasedRememberMeServices rememberMeServices;

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

        // 1. 生成 challenge
        byte[] challengeBytes = new byte[32];
        new SecureRandom().nextBytes(challengeBytes);
        String challenge = Base64.getUrlEncoder().encodeToString(challengeBytes);
        HttpSession session = request.getSession(true);
        if (session != null) {
            session.setAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE, challenge);
        }

        // 2. 构建注册选项
        WebAuthnRegisterOptionsResponseDto dto = new WebAuthnRegisterOptionsResponseDto();
        dto.setChallenge(challenge);

        // 2.1 用户信息
        WebAuthnRegisterOptionsResponseDto.User userDto = new WebAuthnRegisterOptionsResponseDto.User();
        userDto.setId(Base64.getUrlEncoder().encodeToString(userId.getBytes()));
        userDto.setName(user.getUsername());
        userDto.setDisplayName(user.getUsername());
        dto.setUser(userDto);

        // 2.2 RP 信息
        Rp rpDto = new Rp();
        rpDto.setId(getRpId(request));
        rpDto.setName(getRpName());
        dto.setRp(rpDto);

        // 2.3 凭证参数
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

        // 2.4 配置认证器选择
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
     */
    @Override
    @Transactional
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.USER, userOperation = UserOperationType.BIND_MFA, success = "绑定了 Passkey 设备, ID: {{ #id}}")
    public void completeRegistration(String userId,
            WebAuthnRegisterCompleteRequestDto requestDto, HttpServletRequest request) {
        try {
            // 1. 验证 challenge
            HttpSession session = request.getSession(false);
            if (session == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }
            String sessionChallenge = (String) session.getAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE);
            if (sessionChallenge == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 2. 解析并验证 attestationObject
            AttestationValidationResult validationResult = validateAttestation(requestDto, sessionChallenge, request);
            if (!validationResult.isValid()) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1002);
            }

            // 3. 保存凭证到数据库
            WebAuthnCredential credential = new WebAuthnCredential();
            credential.setUserId(userId);
            credential.setCredentialId(requestDto.getId());
            credential.setPublicKey(validationResult.getPublicKeyJson());
            credential.setCounter(0L);
            credential.setDeviceType(getDeviceTypeFromTransports(requestDto.getTransports()));
            credential.setTransports(requestDto.getTransports());
            webAuthnCredentialRepository.save(credential);

            // 4. 清除 session 中的 challenge
            session.removeAttribute(AuthConstants.WEB_AUTHN_REGISTER_CHALLENGE);

            AuditContext.setSpelVariable("id", requestDto.getId());
        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("WebAuthn 注册失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1002);
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
        // 1. 生成 challenge
        byte[] challengeBytes = new byte[32];
        new SecureRandom().nextBytes(challengeBytes);
        String challenge = Base64.getUrlEncoder().encodeToString(challengeBytes);
        request.getSession(true).setAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE, challenge);

        // 2. 构建认证选项
        WebAuthnAuthenticateOptionsResponseDto dto = new WebAuthnAuthenticateOptionsResponseDto();
        dto.setChallenge(challenge);
        dto.setRpId(null);
        dto.setTimeout(60000L);
        dto.setUserVerification("preferred");
        dto.setAllowCredentials(new ArrayList<>());

        if (StringUtils.isNotBlank(userId)) {
            // 2.1 添加用户已存在的凭证
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
            }
        }

        return dto;
    }

    /**
     * 完成认证（登录 和 MFA）
     *
     * @param userId
     *            用户ID（未登录时为 null）
     * @param requestDto
     *            认证完成请求
     * @param request
     *            HTTP请求
     * @param response
     *            HTTP响应
     * @return 认证成功后的用户信息
     */
    @Override
    @Transactional
    public User completeAuthentication(String userId, WebAuthnAuthenticateCompleteRequestDto requestDto,
            HttpServletRequest request, HttpServletResponse response) {
        try {
            // 1. 验证 challenge
            HttpSession session = request.getSession(false);
            if (session == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }
            String sessionChallenge = (String) session.getAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE);
            if (sessionChallenge == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1001);
            }

            // 2. 验证凭证是否属于该用户
            WebAuthnCredential credential = webAuthnCredentialRepository.findByCredentialId(requestDto.getId());
            if (credential == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1003);
            }

            if (StringUtils.isNotBlank(userId) && !credential.getUserId().equals(userId)) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1003);
            }

            // 3. 验证
            // 3.1 解码认证数据
            byte[] authenticatorDataBytes = Base64.getUrlDecoder().decode(requestDto.getResponse());
            byte[] clientDataJSONBytes = Base64.getUrlDecoder().decode(requestDto.getClientDataJSON());
            byte[] signatureBytes = Base64.getUrlDecoder().decode(requestDto.getSignature());
            byte[] credentialIdBytes = Base64.getUrlDecoder().decode(requestDto.getId());

            if (authenticatorDataBytes == null || clientDataJSONBytes == null || signatureBytes == null) {
                throw new BizException(MessageConstants.WEB_AUTHN_MSG_1002);
            }

            // 3.2 构建认证请求
            ServerProperty serverProperty = ServerProperty.builder()
                    .origin(Origin.create(TenantHelper.getTenantConsoleUrl()))
                    .rpId(getRpId(request))
                    .challenge(new DefaultChallenge(sessionChallenge))
                    .build();

            COSEKey coseKey = objectMapper.readValue(credential.getPublicKey(), COSEKey.class);
            AttestedCredentialData attestedCredentialData = new AttestedCredentialData(
                    AAGUID.ZERO, credential.getCredentialId().getBytes(), coseKey);
            CredentialRecord credentialRecord = new CredentialRecordImpl(null, null, null, null,
                    credential.getCounter(), attestedCredentialData, null, null, null, null);

            AuthenticationParameters parameters = new AuthenticationParameters(serverProperty, credentialRecord,
                    List.of(credentialIdBytes), true, true);
            AuthenticationRequest authenticationRequest = new AuthenticationRequest(credentialIdBytes, null,
                    authenticatorDataBytes, clientDataJSONBytes, null, signatureBytes);

            // 3. 验证
            WebAuthnManager webAuthnManager = WebAuthnManager.createNonStrictWebAuthnManager();
            AuthenticationData authenticationData = webAuthnManager.verify(authenticationRequest, parameters);

            // 4. 更新计数器
            if (Objects.nonNull(authenticationData.getAuthenticatorData())) {
                credential.setCounter(authenticationData.getAuthenticatorData().getSignCount());
                credential.setLastUsedAt(LocalDateTime.now());
                webAuthnCredentialRepository.update(credential);
            }

            // 5. 清除 session 中的 challenge
            session.removeAttribute(AuthConstants.WEB_AUTHN_AUTHENTICATE_CHALLENGE);

            // 6. 设置 MFA 验证状态
            setMfaValid(credential.getUserId(), request, response);

            // 7. 返回用户信息
            return userService.getById(credential.getUserId());
        } catch (Exception e) {
            log.error("WebAuthn 认证失败", e);
            throw new BizException(MessageConstants.WEB_AUTHN_MSG_1002, e.getMessage());
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
            // 1. 构建 ServerProperty
            ServerProperty serverProperty = ServerProperty.builder()
                    .origin(Origin.create(TenantHelper.getTenantConsoleUrl()))
                    .rpId(getRpId(request))
                    .challenge(new DefaultChallenge(expectedChallenge))
                    .build();

            // 2. 构建 pubKeyCredParams
            List<PublicKeyCredentialParameters> pubKeyCredParams = Arrays.asList(
                    new PublicKeyCredentialParameters(PublicKeyCredentialType.PUBLIC_KEY,
                            COSEAlgorithmIdentifier.ES256),
                    new PublicKeyCredentialParameters(PublicKeyCredentialType.PUBLIC_KEY,
                            COSEAlgorithmIdentifier.RS256));

            // 3. 构建 RegistrationParameters
            RegistrationParameters parameters = new RegistrationParameters(serverProperty, pubKeyCredParams, true,
                    true);

            // 4. 解析和验证
            WebAuthnManager webAuthnManager = WebAuthnManager.createNonStrictWebAuthnManager();
            RegistrationData registrationData = webAuthnManager.verifyRegistrationResponseJSON(
                    objectMapper.writeValueAsString(requestDto),
                    parameters);

            // 5. 提取公钥
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
     * 列出用户凭证
     */
    @Override
    public List<WebAuthnCredentialResponseDto> listCredentials(String userId) {
        List<WebAuthnCredential> credentials = webAuthnCredentialRepository.findAllByUserId(userId);
        return credentials.stream()
                .map(c -> {
                    WebAuthnCredentialResponseDto dto = new WebAuthnCredentialResponseDto();

                    dto.setId(c.getCredentialId());
                    dto.setDeviceType(c.getDeviceType());
                    dto.setCreatedAt(c.getCreateTime());
                    dto.setLastUsedAt(c.getLastUsedAt());
                    return dto;
                })
                .collect(Collectors.toList());
    }

    /**
     * 删除凭证
     */
    @Override
    @Transactional
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.USER, userOperation = UserOperationType.UNBIND_MFA, success = "删除了 Passkey 设备, ID: {{ #credentialId}}")
    public void deleteCredential(String credentialId, String userId) {
        webAuthnCredentialRepository.deleteByCredentialId(credentialId);
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
     *
     * @return RP 名称
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
     * 设置 MFA 验证结果
     *
     * @param userId
     *            用户ID
     * @param request
     *            HTTP请求
     * @param response
     *            HTTP响应
     */
    private void setMfaValid(String userId, HttpServletRequest request, @Nullable HttpServletResponse response) {
        HttpSession session = request.getSession(true);
        MfaValidContext context = (MfaValidContext) session.getAttribute(AuthConstants.MFA_VALID_CONTEXT);
        if (context != null) {
            context.setValid(true);
            context.addValidatedMethod(AuthConstants.MFA_METHOD_WEBAUTHN);

            if (context.isRememberMeRequested() && Objects.nonNull(response)) {
                rememberMeServices.setRememberMeTokenToCookie(request, response, userId);
            }
        } else {
            MfaValidContext newContext = new MfaValidContext();
            newContext.setValid(true);
            newContext.setUserId(userId);
            newContext.addValidatedMethod(AuthConstants.MFA_METHOD_WEBAUTHN);
            session.setAttribute(AuthConstants.MFA_VALID_CONTEXT, newContext);
        }
    }
}
