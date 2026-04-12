package cn.opensrcdevelop.auth.biz.controller;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.service.permission.PermissionService;
import cn.opensrcdevelop.auth.controller.PermissionController;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(controllers = PermissionController.class)
class PermissionControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private PermissionService permissionService;

    @Test
    @WithMockUser
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
    @WithMockUser
    void getCurrentUserPermissions_shouldReturnEmptyListWhenNoPermissions() throws Exception {
        when(permissionService.getCurrentUserPermissions()).thenReturn(Collections.emptyList());

        mockMvc.perform(get("/api/v1/permissions/me"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$").isArray())
                .andExpect(jsonPath("$").isEmpty());
    }
}