package cn.opensrcdevelop.auth.biz.repository.permission.request;

import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.repository.permission.request.impl.PermissionRequestRepositoryImpl;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
@ActiveProfiles("test")
public class PermissionRequestRepositoryTest {

    @Autowired
    private PermissionRequestMapper permissionRequestMapper;

    private PermissionRequestRepository repository;

    private static final String TENANT_A = "tenant-a";
    private static final String TENANT_B = "tenant-b";

    @BeforeEach
    void setUp() {
        repository = new PermissionRequestRepositoryImpl(permissionRequestMapper);
        // 清理测试数据
        cleanupTestData();
    }

    @AfterEach
    void tearDown() {
        // 清理测试数据
        cleanupTestData();
        TenantContextHolder.removeTenantContext();
    }

    private void cleanupTestData() {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.in(PermissionRequest::getTenantId, TENANT_A, TENANT_B);
        permissionRequestMapper.delete(wrapper);
    }

    private void setTenantContext(String tenantId) {
        TenantContext context = new TenantContext();
        context.setTenantCode(tenantId);
        TenantContextHolder.setTenantContext(context);
    }

    @Test
    void testFindByUserId_TenantIsolation() {
        // 先在 Tenant A 插入数据
        setTenantContext(TENANT_A);
        String userId = UUID.randomUUID().toString();
        insertTestRequest(TENANT_A, userId, "PENDING");

        // 切换到 Tenant B
        setTenantContext(TENANT_B);
        String otherUserId = UUID.randomUUID().toString();
        insertTestRequest(TENANT_B, otherUserId, "PENDING");

        // 在 Tenant B 上下文中查询，应该只返回 Tenant B 的数据
        setTenantContext(TENANT_B);
        List<PermissionRequest> results = repository.findByUserId(otherUserId);

        assertNotNull(results);
        // 验证返回的数据属于 Tenant B
        for (PermissionRequest request : results) {
            assertEquals(TENANT_B, request.getTenantId());
        }
    }

    @Test
    void testFindByStatus_TenantIsolation() {
        // 在 Tenant A 插入待审批数据
        setTenantContext(TENANT_A);
        insertTestRequest(TENANT_A, UUID.randomUUID().toString(), "PENDING");

        // 在 Tenant B 插入已审批数据
        setTenantContext(TENANT_B);
        insertTestRequest(TENANT_B, UUID.randomUUID().toString(), "APPROVED");

        // 在 Tenant A 上下文中查询待审批，应该只返回 Tenant A 的数据
        setTenantContext(TENANT_A);
        List<PermissionRequest> results = repository.findByStatus("PENDING");

        assertNotNull(results);
        // 验证所有返回的数据都是 Tenant A 且状态为 PENDING
        for (PermissionRequest request : results) {
            assertEquals(TENANT_A, request.getTenantId());
            assertEquals("PENDING", request.getStatus());
        }
    }

    @Test
    void testGetById_TenantIsolation() {
        // 在 Tenant A 插入数据
        setTenantContext(TENANT_A);
        String requestId = UUID.randomUUID().toString().replace("-", "");
        insertTestRequestWithId(TENANT_A, requestId, UUID.randomUUID().toString(), "PENDING");

        // 在 Tenant B 上下文中查询该 ID，应该返回 null（跨租户隔离）
        setTenantContext(TENANT_B);
        PermissionRequest result = repository.getById(requestId);

        assertNull(result, "跨租户查询应该返回 null");
    }

    private void insertTestRequest(String tenantId, String userId, String status) {
        insertTestRequestWithId(tenantId, UUID.randomUUID().toString().replace("-", ""), userId, status);
    }

    private void insertTestRequestWithId(String tenantId, String requestId, String userId, String status) {
        PermissionRequest request = new PermissionRequest();
        request.setRequestId(requestId);
        request.setTenantId(tenantId);
        request.setUserId(userId);
        request.setStatus(status);
        request.setRequestTime(java.time.LocalDateTime.now());
        request.setCreateTime(java.time.LocalDateTime.now());
        request.setCreateBy("test");
        request.setVersion(1);
        request.setDeleted(false);
        permissionRequestMapper.insert(request);
    }
}