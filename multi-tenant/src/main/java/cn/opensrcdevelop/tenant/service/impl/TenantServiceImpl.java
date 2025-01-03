package cn.opensrcdevelop.tenant.service.impl;

import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.tenant.constants.MessageConstants;
import cn.opensrcdevelop.tenant.dto.CheckTenantResponseDto;
import cn.opensrcdevelop.tenant.dto.TenantRequestDto;
import cn.opensrcdevelop.tenant.dto.TenantResponseDto;
import cn.opensrcdevelop.tenant.entity.Tenant;
import cn.opensrcdevelop.tenant.mapper.TenantMapper;
import cn.opensrcdevelop.tenant.service.TenantService;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Slf4j
@Service
public class TenantServiceImpl extends ServiceImpl<TenantMapper, Tenant> implements TenantService {

    /**
     * 创建租户
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createTenant(TenantRequestDto requestDto) {
        // 1. 检查租户标识是否存在
        checkTenantCode(requestDto);

        // 1. 创建租户数据库
        TenantHelper.createTenantDatabase(requestDto.getCode());

        // 2. 数据库操作
        Tenant tenant = new Tenant();
        tenant.setTenantId(CommonUtil.getUUIDString());
        tenant.setTenantCode(requestDto.getCode());
        tenant.setTenantName(requestDto.getName());
        tenant.setDescription(requestDto.getDesc());
        CommonUtil.callSetWithCheck(Objects::nonNull, tenant::setEnabled, requestDto::getEnabled);
        super.save(tenant);
    }

    /**
     * 判断租户是否存在
     *
     * @param tenantCode 租户标识
     * @return 是否存在
     */
    @Override
    public Tuple2<Boolean, Tenant> exists(String tenantCode) {
        Tenant tenant =  super.getOne(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getTenantCode, tenantCode).and(o -> o.eq(Tenant::getEnabled, Boolean.TRUE)));
        return Objects.isNull(tenant) ? Tuple.of(false, null) : Tuple.of(true, tenant);
    }

    /**
     * 获取租户列表
     *
     * @param page 页数
     * @param size 条数
     * @param keyword 租户名称 / 标识检索关键字
     * @return 租户列表
     */
    @Override
    public PageData<TenantResponseDto> listTenants(int page, int size, String keyword) {
        // 1. 数据库操作
        List<Tenant> tenants;
        Page<Tenant> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            tenants = super.list(pageRequest, Wrappers.<Tenant>lambdaQuery()
                    .like(Tenant::getTenantName, keyword)
                    .or(o -> o.like(Tenant::getTenantCode, keyword))
                    .orderByDesc(Tenant::getTenantCode));
        } else {
            tenants = super.list(pageRequest, Wrappers.<Tenant>lambdaQuery().orderByDesc(Tenant::getTenantCode));
        }

        // 2. 属性设置
        var records = CommonUtil.stream(tenants).map(tenant -> {
            TenantResponseDto tenantResponse = new TenantResponseDto();
            tenantResponse.setId(tenant.getTenantId());
            tenantResponse.setName(tenant.getTenantName());
            tenantResponse.setCode(tenant.getTenantCode());

            return tenantResponse;
        }).toList();

        PageData<TenantResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());
        pageData.setList(records);
        return pageData;
    }

    /**
     * 更新租户
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updateTenant(TenantRequestDto requestDto) {
        // 1. 获取版本号
        var rawTenant = super.getById(requestDto.getId());
        if (Objects.isNull(rawTenant)) {
            return;
        }

        // 2. 属性设置
        Tenant updateTenant = new Tenant();
        updateTenant.setTenantId(requestDto.getId());
        updateTenant.setTenantName(requestDto.getName());
        updateTenant.setDescription(requestDto.getDesc());
        CommonUtil.callSetWithCheck(Objects::nonNull, updateTenant::setEnabled, requestDto::getEnabled);
        updateTenant.setVersion(rawTenant.getVersion());

        // 3. 数据据操作
        super.updateById(updateTenant);
    }

    /**
     * 获取租户详情
     *
     * @param tenantId 租户 ID
     * @return 租户详情
     */
    @Override
    public TenantResponseDto detail(String tenantId) {
        // 1. 数据库操作
        Tenant tenant = super.getById(tenantId);
        String tenantCode = tenant.getTenantCode();

        // 2. 属性设置
        TenantResponseDto tenantResponse = new TenantResponseDto();
        tenantResponse.setId(tenant.getTenantId());
        tenantResponse.setName(tenant.getTenantName());
        tenantResponse.setCode(tenantCode);
        tenantResponse.setDesc(tenant.getDescription());
        tenantResponse.setEnabled(tenant.getEnabled());
        tenantResponse.setIssuer(TenantHelper.getTenantIssuer(tenantCode));
        tenantResponse.setConsoleUrl(TenantHelper.getTenantConsoleUrl(tenantCode));
        tenantResponse.setCreateTime(tenant.getCreateTime());
        return tenantResponse;
    }

    /**
     * 删除租户
     *
     * @param tenantId 租户 ID
     */
    @Transactional
    @Override
    public void removeTenant(String tenantId) {
        // 1. 获取租户信息
        Tenant tenant = super.getById(tenantId);
        if (Objects.isNull(tenant)) {
            return;
        }

        // 2. 删除租户数据库
        TenantHelper.removeTenantDatabase(tenant.getTenantCode());

        // 3. 数据库操作
        super.removeById(tenantId);
    }

    /**
     * 检查租户
     *
     * @param tenantCode 租户标识
     * @return 检查结果
     */
    @Override
    public CheckTenantResponseDto checkTenant(String tenantCode){
        try {
            CheckTenantResponseDto checkTenantResponse = new CheckTenantResponseDto();
            var existsRes = TenantHelper.tenantExists(tenantCode);
            if (Boolean.TRUE.equals(existsRes._1)) {
                checkTenantResponse.setExists(true);
                checkTenantResponse.setIssuer(TenantHelper.getTenantIssuer(tenantCode));
                checkTenantResponse.setTenantName(existsRes._2.getTenantName());
            } else {
                checkTenantResponse.setExists(false);
            }
            return checkTenantResponse;
        } catch (Exception e) {
            throw new ServerException(e);
        }
    }

    private void checkTenantCode(TenantRequestDto requestDto) {
        if (Objects.nonNull(super.getOne(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getTenantCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.TENANT_MSG_1001, requestDto.getCode());
        }
    }
}
