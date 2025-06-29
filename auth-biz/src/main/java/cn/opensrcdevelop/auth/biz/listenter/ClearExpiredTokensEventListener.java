package cn.opensrcdevelop.auth.biz.listenter;

import cn.opensrcdevelop.auth.biz.component.DbOAuth2AuthorizationService;
import cn.opensrcdevelop.auth.biz.event.ClearExpiredTokensEvent;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.security.oauth2.server.authorization.OAuth2Authorization;
import org.springframework.security.oauth2.server.authorization.OAuth2AuthorizationCode;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
@Slf4j
public class ClearExpiredTokensEventListener implements ApplicationListener<ClearExpiredTokensEvent> {

    private final DbOAuth2AuthorizationService dbOAuth2AuthorizationService;

    @Override
    @Async(ExecutorConstants.EXECUTOR_IO_DENSE)
    public void onApplicationEvent(ClearExpiredTokensEvent event) {
        // 异步场合，需要切换租户数据源
        TenantHelper.switchTenantDs(TenantContextHolder.getTenantContext().getTenantCode());
        log.info("开始清除过期 token");

        // 1. 获取客户端下所有授权的 token
        String clientId = (String) event.getSource();
        var oauth2AuthorizationList = dbOAuth2AuthorizationService.findByClientId(clientId);

        if (CollectionUtils.isEmpty(oauth2AuthorizationList)) {
            log.info("结束清除过期 token");
            return;
        }

        // 2. 清除 token，生命周期由长到短依次清除（refresh_token → access_token → authorization_code）
        List<OAuth2Authorization> deleteTargetList = new ArrayList<>();
        // 2.1 清除 refresh_token 过期的记录
        clearRefreshToken(oauth2AuthorizationList, deleteTargetList);

        // 2.2 清除 access_token 过期的记录
        // 2.2.1. 清除无 refresh_token，access_token 过期的记录
        clearAccessTokenWithoutRefreshToken(oauth2AuthorizationList, deleteTargetList);

        // 2.2.2 清除有 refresh_token，access_token 过期的记录
        clearAccessTokenWithRefreshToken(oauth2AuthorizationList, deleteTargetList);

        // 2.3 清除 authorization_code 过期的记录
        clearAuthorizationCode(oauth2AuthorizationList, deleteTargetList);

        // 3. 数据库操作
        var deleteTargetIds = CommonUtil.stream(deleteTargetList).map(OAuth2Authorization::getId).map(Long::parseLong).toList();
        log.info("清除的过期 token 数：{}", deleteTargetIds.size());
        if (CollectionUtils.isNotEmpty(deleteTargetIds)) {
            dbOAuth2AuthorizationService.removeByIds(deleteTargetIds);
        }
        log.info("结束清除过期 token");
    }

    /**
     * 清除 refresh_token 过期的记录
     */
    private void clearRefreshToken(List<OAuth2Authorization> oAuth2AuthorizationList, List<OAuth2Authorization> deleteTargetList) {
        oAuth2AuthorizationList.forEach(authorization -> {
            if (Objects.nonNull(authorization.getRefreshToken()) && authorization.getRefreshToken().isExpired()) {
                deleteTargetList.add(authorization);
            }
        });
        oAuth2AuthorizationList.removeAll(deleteTargetList);
    }

    /**
     * 清除无 refresh_token，access_token 过期的记录
     */
    private void clearAccessTokenWithoutRefreshToken(List<OAuth2Authorization> oAuth2AuthorizationList, List<OAuth2Authorization> deleteTargetList) {
        oAuth2AuthorizationList.forEach(authorization -> {
            if (Objects.nonNull(authorization.getAccessToken()) && Objects.isNull(authorization.getRefreshToken()) && authorization.getAccessToken().isExpired()) {
                deleteTargetList.add(authorization);
            }
        });
        oAuth2AuthorizationList.removeAll(deleteTargetList);
    }

    /**
     * 清除有 refresh_token，access_token 过期的记录
     */
    private void clearAccessTokenWithRefreshToken(List<OAuth2Authorization> oAuth2AuthorizationList, List<OAuth2Authorization> deleteTargetList) {
        // 2.2.2.1 按 refresh_token 分组，清除 access_token 过期的记录
        var groupRes = CommonUtil.stream(oAuth2AuthorizationList)
                .filter(o -> Objects.nonNull(o.getRefreshToken()))
                .collect(Collectors.groupingBy(OAuth2Authorization::getRefreshToken));
        if (MapUtils.isNotEmpty(groupRes)) {
            // 2.2.2.1.1 分组内按 ID 排序（升序）
            groupRes.keySet().forEach(key -> groupRes.computeIfPresent(key, (k, v) -> v.stream().sorted(Comparator.comparing(OAuth2Authorization::getId)).collect(Collectors.toCollection(ArrayList::new))));
            // 2.2.2.1.2 保留分组内最新的一条记录，即保留 refresh_token
            groupRes.keySet().forEach(key -> groupRes.computeIfPresent(key, (k, v) -> {
                if (!v.isEmpty()) {
                    v.removeLast();
                }
                return v;
            }));

            // 2.2.2.1.3 清除 access_token 过期的记录
            CommonUtil.stream(groupRes.values()).forEach(authorizationList -> authorizationList.forEach(authorization -> {
                if (Objects.nonNull(authorization.getAccessToken()) && authorization.getAccessToken().isExpired()) {
                    deleteTargetList.add(authorization);
                }
            }));
            oAuth2AuthorizationList.removeAll(deleteTargetList);
        }
    }

    /**
     * 清除 authorization_code 过期的记录
     */
    private void clearAuthorizationCode(List<OAuth2Authorization> oAuth2AuthorizationList, List<OAuth2Authorization> deleteTargetList) {
        oAuth2AuthorizationList.forEach(authorization -> {
            if (Objects.isNull(authorization.getAccessToken()) && Objects.isNull(authorization.getRefreshToken())) {
                var code = authorization.getToken(OAuth2AuthorizationCode.class);
                if (Objects.nonNull(code) && code.isExpired()) {
                    deleteTargetList.add(authorization);
                }
            }
        });
        oAuth2AuthorizationList.removeAll(deleteTargetList);
    }
}
