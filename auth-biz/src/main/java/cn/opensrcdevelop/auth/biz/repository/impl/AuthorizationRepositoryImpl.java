package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.Authorization;
import cn.opensrcdevelop.auth.biz.mapper.AuthorizationMapper;
import cn.opensrcdevelop.auth.biz.repository.AuthorizationRepository;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class AuthorizationRepositoryImpl implements AuthorizationRepository {

    private final AuthorizationMapper mapper;

    @Override
    public Authorization findByStateOrAuthorizationCodeValueOrAccessTokenValueOrRefreshTokenValueOrOidcIdTokenValueOrUserCodeValueOrDeviceCodeValue(String token) {
        List<Authorization> authorizationList = mapper.selectList(Wrappers.<Authorization>lambdaQuery()
                .eq(Authorization::getState, token).or()
                .eq(Authorization::getAuthorizationCodeValue, token).or()
                .eq(Authorization::getAccessTokenValue, token).or()
                .eq(Authorization::getRefreshTokenValue, token).or()
                .eq(Authorization::getOidcIdTokenValue, token).or()
                .eq(Authorization::getUserCodeValue, token).or()
                .eq(Authorization::getDeviceCodeValue, token)
                .orderByDesc(Authorization::getId));
        if (CollectionUtils.isNotEmpty(authorizationList)) {
            return authorizationList.get(0);
        }
        return null;
    }

    @Override
    public Authorization findByState(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getState, token));
    }

    @Override
    public Authorization findByAuthorizationCodeValue(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getAuthorizationCodeValue, token));
    }

    @Override
    public Authorization findByAccessTokenValue(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getAccessTokenValue, token));
    }

    @Override
    public Authorization findByRefreshTokenValue(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getRefreshTokenValue, token).orderByDesc(Authorization::getId));
    }

    @Override
    public Authorization findByOidcIdTokenValue(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getOidcIdTokenValue, token));
    }

    @Override
    public Authorization findByUserCodeValue(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getUserCodeValue, token));
    }

    @Override
    public Authorization findByDeviceCodeValue(String token) {
        return mapper.selectOne(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getDeviceCodeValue, token));
    }

    @Override
    public Authorization findById(Long id) {
        return mapper.selectById(id);
    }

    @Override
    public void deleteById(Long id) {
        mapper.deleteById(id);
    }

    @Override
    public void save(Authorization authorization) {
        mapper.insert(authorization);
    }

    @Override
    public void deleteUserTokens(String principalName) {
        mapper.delete(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getPrincipalName, principalName));
    }

    @Override
    public List<Authorization> getAuthorizationsByClientId(String clientId) {
        return mapper.selectList(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getRegisteredClientId, clientId));
    }

    @Override
    public void deleteByIds(List<Long> ids) {
        mapper.deleteByIds(ids);
    }

    @Override
    public void deleteByLoginId(String loginId) {
        mapper.delete(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getLoginId, loginId));
    }

    @Override
    public List<Authorization> findRefreshTokensByPrincipalName(String principalName) {
        return mapper.selectList(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getPrincipalName, principalName).and(o -> o.isNotNull(Authorization::getRefreshTokenValue)));
    }

    @Override
    public List<Authorization> findRefreshTokensByLoginId(String loginId) {
        return mapper.selectList(Wrappers.<Authorization>lambdaQuery().eq(Authorization::getLoginId, loginId).and(o -> o.isNotNull(Authorization::getRefreshTokenValue)));
    }
}
