package cn.opensrcdevelop.auth.biz.service.permission.request;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequestItem;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestItemMapper;
import cn.opensrcdevelop.auth.biz.mapper.permission.request.PermissionRequestMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.request.PermissionRequestRepository;
import cn.opensrcdevelop.auth.biz.service.auth.AuthorizeService;
import cn.opensrcdevelop.auth.biz.service.permission.request.impl.PermissionRequestServiceImpl;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class PermissionRequestServiceTest {

    private PermissionRequestMapper permissionRequestMapper;
    private PermissionRequestItemMapper permissionRequestItemMapper;
    private PermissionAutoApproveService permissionAutoApproveService;
    private AuthorizeService authorizeService;
    private PermissionRequestRepository permissionRequestRepository;
    private PermissionRequestServiceImpl permissionRequestService;

    @BeforeEach
    void setUp() {
        permissionRequestMapper = mock(PermissionRequestMapper.class);
        permissionRequestItemMapper = mock(PermissionRequestItemMapper.class);
        permissionAutoApproveService = mock(PermissionAutoApproveService.class);
        authorizeService = mock(AuthorizeService.class);
        permissionRequestRepository = mock(PermissionRequestRepository.class);
        permissionRequestService = new PermissionRequestServiceImpl(
                permissionRequestMapper,
                permissionRequestItemMapper,
                permissionAutoApproveService,
                authorizeService,
                permissionRequestRepository);
    }

    @Test
    void submitRequest_allPending_shouldReturnCorrectCounts() {
        PermissionRequestCreateDto dto = new PermissionRequestCreateDto();
        dto.setPermissionIds(List.of("p1", "p2"));
        dto.setReason("业务需要");

        when(permissionAutoApproveService.isEnabled("p1")).thenReturn(false);
        when(permissionAutoApproveService.isEnabled("p2")).thenReturn(false);
        when(permissionRequestRepository.hasActivePendingRequest(anyString(), anyList())).thenReturn(false);
        when(permissionRequestMapper.insert(any(PermissionRequest.class))).thenReturn(1);
        when(permissionRequestItemMapper.insert(any(PermissionRequestItem.class))).thenReturn(1);

        try (MockedStatic<AuthUtil> authUtil = mockStatic(AuthUtil.class)) {
            authUtil.when(AuthUtil::getCurrentUserId).thenReturn("user-001");

            PermissionRequestResponseDto result = permissionRequestService.submitRequest(dto);

            assertNotNull(result.getRequestId());
            assertEquals(0, result.getAutoApprovedCount());
            assertEquals(2, result.getPendingCount());
        }
    }

    @Test
    void submitRequest_partialAutoApprove_shouldCallAuthorizeService() {
        PermissionRequestCreateDto dto = new PermissionRequestCreateDto();
        dto.setPermissionIds(List.of("p1", "p2"));
        dto.setReason("业务需要");

        when(permissionAutoApproveService.isEnabled("p1")).thenReturn(true);
        when(permissionAutoApproveService.isEnabled("p2")).thenReturn(false);
        when(permissionRequestRepository.hasActivePendingRequest(anyString(), anyList())).thenReturn(false);
        when(permissionRequestMapper.insert(any(PermissionRequest.class))).thenReturn(1);
        when(permissionRequestItemMapper.insert(any(PermissionRequestItem.class))).thenReturn(1);

        try (MockedStatic<AuthUtil> authUtil = mockStatic(AuthUtil.class)) {
            authUtil.when(AuthUtil::getCurrentUserId).thenReturn("user-001");

            PermissionRequestResponseDto result = permissionRequestService.submitRequest(dto);

            assertEquals(1, result.getAutoApprovedCount());
            assertEquals(1, result.getPendingCount());
            verify(authorizeService, times(1)).authorize(
                    argThat(req -> req.getUserIds().contains("user-001")
                            && req.getPermissionIds().contains("p1")
                            && !req.getPermissionIds().contains("p2")));
        }
    }

    @Test
    void submitRequest_allAutoApprove_shouldSetStatusAutoApproved() {
        PermissionRequestCreateDto dto = new PermissionRequestCreateDto();
        dto.setPermissionIds(List.of("p1"));
        dto.setReason("业务需要");

        when(permissionAutoApproveService.isEnabled("p1")).thenReturn(true);
        when(permissionRequestRepository.hasActivePendingRequest(anyString(), anyList())).thenReturn(false);
        when(permissionRequestMapper.insert(any(PermissionRequest.class))).thenReturn(1);
        when(permissionRequestItemMapper.insert(any(PermissionRequestItem.class))).thenReturn(1);

        ArgumentCaptor<PermissionRequest> captor = ArgumentCaptor.forClass(PermissionRequest.class);

        try (MockedStatic<AuthUtil> authUtil = mockStatic(AuthUtil.class)) {
            authUtil.when(AuthUtil::getCurrentUserId).thenReturn("user-001");

            PermissionRequestResponseDto result = permissionRequestService.submitRequest(dto);

            assertEquals(1, result.getAutoApprovedCount());
            assertEquals(0, result.getPendingCount());
            verify(permissionRequestMapper).insert(captor.capture());
            assertEquals(PermissionRequestStatusEnum.AUTO_APPROVED.getCode(), captor.getValue().getStatus());
        }
    }

    @Test
    void submitRequest_duplicatePendingRequest_shouldThrowException() {
        PermissionRequestCreateDto dto = new PermissionRequestCreateDto();
        dto.setPermissionIds(List.of("p1"));
        dto.setReason("业务需要");

        when(permissionRequestRepository.hasActivePendingRequest(anyString(), anyList())).thenReturn(true);

        try (MockedStatic<AuthUtil> authUtil = mockStatic(AuthUtil.class)) {
            authUtil.when(AuthUtil::getCurrentUserId).thenReturn("user-001");

            assertThrows(IllegalStateException.class, () -> permissionRequestService.submitRequest(dto));
            verify(permissionRequestMapper, never()).insert(any(PermissionRequest.class));
        }
    }
}
