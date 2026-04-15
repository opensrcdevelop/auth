package cn.opensrcdevelop.auth.biz.controller;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

@Disabled("Pre-existing broken test: PermissionController is in auth-server, spring-security-test not in classpath")
@WebMvcTest
class PermissionControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private PermissionService permissionService;

    @Test
    void getCurrentUserPermissions_shouldReturnUserPermissions() throws Exception {
        PermissionResponseDto dto = new PermissionResponseDto();
        dto.setPermissionId("perm-001");
        dto.setPermissionName("测试权限");
        dto.setPermissionCode("test:read");
        dto.setPermissionLocator("TEST:RES:READ");

        when(permissionService.getCurrentUserPermissions()).thenReturn(List.of(dto));

        mockMvc.perform(get("/api/v1/permissions/me"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$[0].permissionId").value("perm-001"))
                .andExpect(jsonPath("$[0].permissionLocator").value("TEST:RES:READ"));
    }

    @Test
    void getCurrentUserPermissions_shouldReturnEmptyListWhenNoPermissions() throws Exception {
        when(permissionService.getCurrentUserPermissions()).thenReturn(Collections.emptyList());

        mockMvc.perform(get("/api/v1/permissions/me"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$").isEmpty());
    }
}
