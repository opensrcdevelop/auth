package cn.opensrcdevelop.auth.biz.component.oauth2login;

import java.util.Map;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthorizationCodeAuthenticationProvider;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthorizationCodeAuthenticationToken;
import org.springframework.security.oauth2.client.authentication.OAuth2LoginAuthenticationToken;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2AuthorizationException;
import org.springframework.security.oauth2.core.OAuth2Error;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.util.Assert;

public class CustomOAuth2LoginAuthenticationProvider implements AuthenticationProvider {

    private final OAuth2AuthorizationCodeAuthenticationProvider authorizationCodeAuthenticationProvider;
    private final OAuth2UserService<OAuth2UserRequest, OAuth2User> userService;

    public CustomOAuth2LoginAuthenticationProvider(
            OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> accessTokenResponseClient,
            OAuth2UserService<OAuth2UserRequest, OAuth2User> userService) {
        Assert.notNull(userService, "userService cannot be null");
        this.authorizationCodeAuthenticationProvider = new OAuth2AuthorizationCodeAuthenticationProvider(
                accessTokenResponseClient);
        this.userService = userService;
    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        OAuth2LoginAuthenticationToken loginAuthenticationToken = (OAuth2LoginAuthenticationToken) authentication;
        OAuth2AuthorizationCodeAuthenticationToken authorizationCodeAuthenticationToken;
        try {
            authorizationCodeAuthenticationToken = (OAuth2AuthorizationCodeAuthenticationToken) this.authorizationCodeAuthenticationProvider
                    .authenticate(
                            new OAuth2AuthorizationCodeAuthenticationToken(
                                    loginAuthenticationToken.getClientRegistration(),
                                    loginAuthenticationToken.getAuthorizationExchange()));
        } catch (OAuth2AuthorizationException ex) {
            OAuth2Error oauth2Error = ex.getError();
            throw new OAuth2AuthenticationException(oauth2Error, oauth2Error.toString(), ex);
        }
        OAuth2AccessToken accessToken = authorizationCodeAuthenticationToken.getAccessToken();
        Map<String, Object> additionalParameters = authorizationCodeAuthenticationToken.getAdditionalParameters();
        OAuth2User oauth2User = this.userService.loadUser(new OAuth2UserRequest(
                loginAuthenticationToken.getClientRegistration(), accessToken, additionalParameters));
        OAuth2LoginAuthenticationToken authenticationResult = new OAuth2LoginAuthenticationToken(
                loginAuthenticationToken.getClientRegistration(), loginAuthenticationToken.getAuthorizationExchange(),
                oauth2User, oauth2User.getAuthorities(), accessToken,
                authorizationCodeAuthenticationToken.getRefreshToken());
        authenticationResult.setDetails(loginAuthenticationToken.getDetails());
        return authenticationResult;
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return OAuth2LoginAuthenticationToken.class.isAssignableFrom(authentication);
    }
}
