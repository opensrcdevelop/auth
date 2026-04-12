package cn.opensrcdevelop.auth.controller.user;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestCreateDto;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestResponseDto;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestService;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class UserCenterPermissionRequestControllerTest {

    private PermissionRequestService permissionRequestService;
    private UserCenterPermissionRequestController controller;

    @BeforeEach
    void setUp() {
        permissionRequestService = mock(PermissionRequestService.class);
        controller = new UserCenterPermissionRequestController(permissionRequestService);
    }

    @Test
    void submitRequest_validInput_shouldReturnResponseWithRequestId() {
        PermissionRequestResponseDto expectedResponse = new PermissionRequestResponseDto();
        expectedResponse.setRequestId("req-001");
        expectedResponse.setAutoApprovedCount(0);
        expectedResponse.setPendingCount(1);

        when(permissionRequestService.submitRequest(any(PermissionRequestCreateDto.class)))
                .thenReturn(expectedResponse);

        PermissionRequestCreateDto requestDto = new PermissionRequestCreateDto();
        requestDto.setPermissionIds(List.of("perm-001"));
        requestDto.setReason("业务需要");

        PermissionRequestResponseDto result = controller.submitRequest(requestDto);

        assertNotNull(result);
        assertEquals("req-001", result.getRequestId());
        assertEquals(0, result.getAutoApprovedCount());
        assertEquals(1, result.getPendingCount());

        ArgumentCaptor<PermissionRequestCreateDto> captor = ArgumentCaptor.forClass(PermissionRequestCreateDto.class);
        verify(permissionRequestService).submitRequest(captor.capture());
        assertEquals(List.of("perm-001"), captor.getValue().getPermissionIds());
        assertEquals("业务需要", captor.getValue().getReason());
    }

    @Test
    void submitRequest_autoApprovedPermissions_shouldReturnCorrectCounts() {
        PermissionRequestResponseDto expectedResponse = new PermissionRequestResponseDto();
        expectedResponse.setRequestId("req-002");
        expectedResponse.setAutoApprovedCount(1);
        expectedResponse.setPendingCount(1);

        when(permissionRequestService.submitRequest(any(PermissionRequestCreateDto.class)))
                .thenReturn(expectedResponse);

        PermissionRequestCreateDto requestDto = new PermissionRequestCreateDto();
        requestDto.setPermissionIds(List.of("p1", "p2"));
        requestDto.setReason("需要访问数据");

        PermissionRequestResponseDto result = controller.submitRequest(requestDto);

        assertNotNull(result);
        assertEquals("req-002", result.getRequestId());
        assertEquals(1, result.getAutoApprovedCount());
        assertEquals(1, result.getPendingCount());
    }
}
