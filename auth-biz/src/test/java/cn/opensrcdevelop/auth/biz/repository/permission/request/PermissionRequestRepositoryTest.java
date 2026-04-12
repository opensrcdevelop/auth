package cn.opensrcdevelop.auth.biz.repository.permission.request;

import static org.junit.jupiter.api.Assertions.*;

import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.request.impl.PermissionRequestRepositoryImpl;
import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
public class PermissionRequestRepositoryTest {

    @Autowired
    private PermissionRequestMapper permissionRequestMapper;

    @Autowired
    private PermissionRequestItemMapper permissionRequestItemMapper;

    private PermissionRequestRepository repository;

    private static final String TEST_TENANT = "master";

    @BeforeEach
    void setUp() {
        repository = new PermissionRequestRepositoryImpl(permissionRequestMapper, permissionRequestItemMapper);
        // 设置租户上下文（per-tenant 模式下由数据源切换实现隔离）
        setTenantContext(TEST_TENANT);
        // 清理测试数据
        cleanupTestData();
    }

    @AfterEach
    void tearDown() {
        // 清理测试数据
        cleanupTestData();
        TenantContextHolder.removeTenantContext();
    }

    private void setTenantContext(String tenantId) {
        TenantContext context = new TenantContext();
        context.setTenantCode(tenantId);
        TenantContextHolder.setTenantContext(context);
    }

    private void cleanupTestData() {
        LambdaQueryWrapper<PermissionRequest> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(PermissionRequest::getUserId, "test-user");
        permissionRequestMapper.delete(wrapper);
    }

    @Test
    void testGetById() {
        String requestId = UUID.randomUUID().toString().replace("-", "");
        insertTestRequest(requestId, "test-user", "PENDING");

        PermissionRequest result = repository.getById(requestId);

        assertNotNull(result);
        assertEquals(requestId, result.getRequestId());
        assertEquals("test-user", result.getUserId());
        assertEquals("PENDING", result.getStatus());
    }

    @Test
    void testGetById_NotFound() {
        PermissionRequest result = repository.getById("non-existent-id");
        assertNull(result);
    }

    @Test
    void testFindByUserId() {
        String userId = "test-user";
        insertTestRequest(UUID.randomUUID().toString().replace("-", ""), userId, "PENDING");
        insertTestRequest(UUID.randomUUID().toString().replace("-", ""), userId, "APPROVED");
        insertTestRequest(UUID.randomUUID().toString().replace("-", ""), "other-user", "PENDING");

        List<PermissionRequest> results = repository.findByUserId(userId);

        assertNotNull(results);
        assertEquals(2, results.size());
        for (PermissionRequest request : results) {
            assertEquals(userId, request.getUserId());
        }
    }

    @Test
    void testFindByStatus() {
        String userId = "test-user";
        insertTestRequest(UUID.randomUUID().toString().replace("-", ""), userId, "PENDING");
        insertTestRequest(UUID.randomUUID().toString().replace("-", ""), userId, "PENDING");
        insertTestRequest(UUID.randomUUID().toString().replace("-", ""), userId, "APPROVED");

        List<PermissionRequest> results = repository.findByStatus("PENDING");

        assertNotNull(results);
        assertEquals(2, results.size());
        for (PermissionRequest request : results) {
            assertEquals("PENDING", request.getStatus());
        }
    }

    @Test
    void testFindByStatus_Pagination() {
        String userId = "test-user";
        for (int i = 0; i < 5; i++) {
            insertTestRequest(UUID.randomUUID().toString().replace("-", ""), userId, "PENDING");
        }

        // 分页查询
        cn.opensrcdevelop.common.response.PageData<PermissionRequest> pageData = repository.findByStatus("PENDING", 1,
                3);

        assertNotNull(pageData);
        assertEquals(5, pageData.getTotal());
        assertEquals(3, pageData.getList().size());
    }

    @Test
    void testInsertAndDelete() {
        String requestId = UUID.randomUUID().toString().replace("-", "");
        insertTestRequest(requestId, "test-user", "PENDING");

        PermissionRequest result = repository.getById(requestId);
        assertNotNull(result);

        permissionRequestMapper.deleteById(requestId);
        result = repository.getById(requestId);
        assertNull(result);
    }

    private void insertTestRequest(String requestId, String userId, String status) {
        PermissionRequest request = new PermissionRequest();
        request.setRequestId(requestId);
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
