package cn.opensrcdevelop.tenant.service;

import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.tenant.dto.CheckTenantResponseDto;
import cn.opensrcdevelop.tenant.dto.TenantRequestDto;
import cn.opensrcdevelop.tenant.dto.TenantResponseDto;
import cn.opensrcdevelop.tenant.entity.Tenant;
import com.baomidou.mybatisplus.extension.service.IService;
import io.vavr.Tuple2;

public interface TenantService extends IService<Tenant> {

    void createTenant(TenantRequestDto requestDto);

    Tuple2<Boolean, Tenant> exists(String tenantCode);

    PageData<TenantResponseDto> listTenants(int page, int size, String keyword);

    void updateTenant(TenantRequestDto requestDto);

    TenantResponseDto detail(String tenantId);

    void removeTenant(String tenantId);

    CheckTenantResponseDto checkTenant(String tenantCode);
}
