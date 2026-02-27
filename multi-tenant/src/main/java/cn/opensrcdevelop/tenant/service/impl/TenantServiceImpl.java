package cn.opensrcdevelop.tenant.service.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
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
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class TenantServiceImpl extends ServiceImpl<TenantMapper, Tenant> implements TenantService {

    /**
     * 创建租户
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.TENANT, sysOperation = SysOperationType.CREATE, success = "创建了租户（{{ @linkGen.toLink(#tenantId, T(ResourceType).TENANT) }}）", fail = "'创建租户（{{ #requestDto.name }}）失败")
    @Transactional
    @Override
    public void createTenant(TenantRequestDto requestDto) {
        // 1. 检查租户标识是否存在
        checkTenantCode(requestDto);

        // 2. 检查时间有效性
        checkEffectiveTime(requestDto);

        // 3. 创建租户数据库
        TenantHelper.createTenantDatabase(requestDto.getCode());

        // 2. 数据库操作
        String tenantId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("tenantId", tenantId);

        Tenant tenant = new Tenant();
        tenant.setTenantId(tenantId);
        tenant.setTenantCode(requestDto.getCode());
        tenant.setTenantName(requestDto.getName());
        tenant.setDescription(requestDto.getDesc());
        CommonUtil.callSetWithCheck(Objects::nonNull, tenant::setEnabled, requestDto::getEnabled);
        CommonUtil.callSetWithCheck(Objects::nonNull, tenant::setEffectiveTime, requestDto::getEffectiveTime);
        CommonUtil.callSetWithCheck(Objects::nonNull, tenant::setExpirationTime, requestDto::getExpirationTime);
        super.save(tenant);
    }

    /**
     * 判断租户是否存在
     *
     * @param tenantCode
     *            租户标识
     * @return 是否存在
     */
    @Override
    public Tuple2<Boolean, Tenant> exists(String tenantCode) {
        Tenant tenant = super.getOne(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getTenantCode, tenantCode)
                .and(o -> o.eq(Tenant::getEnabled, Boolean.TRUE)));
        if (Objects.isNull(tenant)) {
            return Tuple.of(false, null);
        }
        // 检查租户是否在有效期内
        if (!isTenantEffective(tenant)) {
            return Tuple.of(false, tenant);
        }
        return Tuple.of(true, tenant);
    }

    /**
     * 检查租户是否在有效期内
     *
     * @param tenant
     *            租户
     * @return 是否有效
     */
    private boolean isTenantEffective(Tenant tenant) {
        LocalDateTime now = LocalDateTime.now();
        // 检查生效时间
        if (Objects.nonNull(tenant.getEffectiveTime()) && now.isBefore(tenant.getEffectiveTime())) {
            return false;
        }
        // 检查失效时间
        if (Objects.nonNull(tenant.getExpirationTime()) && now.isAfter(tenant.getExpirationTime())) {
            return false;
        }
        return true;
    }

    /**
     * 获取租户列表
     *
     * @param page
     *            页数
     * @param size
     *            条数
     * @param keyword
     *            租户名称 / 标识检索关键字
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
            tenantResponse.setEffectiveTime(tenant.getEffectiveTime());
            tenantResponse.setExpirationTime(tenant.getExpirationTime());

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
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.TENANT, sysOperation = SysOperationType.UPDATE, success = "修改了租户（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).TENANT) }}）", fail = "修改租户（{{ @linkGen.toLink(#requestDto.id, T(ResourceType).TENANT) }}）失败")
    @Transactional
    @Override
    public void updateTenant(TenantRequestDto requestDto) {
        String tenantId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取版本号
        var rawTenant = super.getById(tenantId);
        if (Objects.isNull(rawTenant)) {
            return;
        }

        // 2. 检查时间有效性
        checkEffectiveTime(requestDto);

        compareObjBuilder.id(tenantId);
        compareObjBuilder.before(rawTenant);

        // 3. 属性设置
        Tenant updateTenant = new Tenant();
        updateTenant.setTenantId(requestDto.getId());
        updateTenant.setTenantName(requestDto.getName());
        updateTenant.setDescription(requestDto.getDesc());
        CommonUtil.callSetWithCheck(Objects::nonNull, updateTenant::setEnabled, requestDto::getEnabled);
        // 支持将时间字段设置为 null
        updateTenant.setEffectiveTime(requestDto.getEffectiveTime());
        updateTenant.setExpirationTime(requestDto.getExpirationTime());
        updateTenant.setVersion(rawTenant.getVersion());

        // 3. 数据据操作
        super.updateById(updateTenant);

        // 4. 执行数据库迁移
        if (Boolean.TRUE.equals(updateTenant.getEnabled())) {
            DataSource dataSource = TenantHelper.createTenantDataSource(rawTenant.getTenantCode());
            TenantHelper.executeTenantDbMigrations(dataSource, Collections.emptyMap());
        }

        // 5. 删除租户数据源
        if (Boolean.FALSE.equals(updateTenant.getEnabled())) {
            TenantHelper.removeTenantDs(rawTenant.getTenantCode());
        }

        compareObjBuilder.after(super.getById(tenantId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 获取租户详情
     *
     * @param tenantId
     *            租户 ID
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
        tenantResponse.setEffectiveTime(tenant.getEffectiveTime());
        tenantResponse.setExpirationTime(tenant.getExpirationTime());
        tenantResponse.setIssuer(TenantHelper.getTenantIssuer(tenantCode));
        tenantResponse.setConsoleUrl(TenantHelper.getTenantConsoleUrl(tenantCode));
        tenantResponse.setCreateTime(tenant.getCreateTime());
        return tenantResponse;
    }

    /**
     * 删除租户
     *
     * @param tenantId
     *            租户 ID
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.TENANT, sysOperation = SysOperationType.DELETE, success = "删除了租户（{{ @linkGen.toLink(#tenantId, T(ResourceType).TENANT) }}）", fail = "删除租户（{{ @linkGen.toLink(#tenantId, T(ResourceType).TENANT) }}）失败")
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
     * @param tenantCode
     *            租户标识
     * @return 检查结果
     */
    @Override
    public CheckTenantResponseDto checkTenant(String tenantCode) {
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
        if (Objects.nonNull(
                super.getOne(Wrappers.<Tenant>lambdaQuery().eq(Tenant::getTenantCode, requestDto.getCode())))) {
            throw new BizException(MessageConstants.TENANT_MSG_1001, requestDto.getCode());
        }
    }

    /**
     * 检查时间有效性：生效时间必须早于失效时间
     *
     * @param requestDto
     *            请求
     */
    private void checkEffectiveTime(TenantRequestDto requestDto) {
        LocalDateTime effectiveTime = requestDto.getEffectiveTime();
        LocalDateTime expirationTime = requestDto.getExpirationTime();
        // 如果两者都为空，或者只有生效时间，或者只有失效时间，都认为是合法的
        if (Objects.isNull(effectiveTime) || Objects.isNull(expirationTime)) {
            return;
        }
        // 生效时间必须早于失效时间
        if (effectiveTime.isAfter(expirationTime)) {
            throw new BizException(MessageConstants.TENANT_MSG_1002);
        }
    }
}
