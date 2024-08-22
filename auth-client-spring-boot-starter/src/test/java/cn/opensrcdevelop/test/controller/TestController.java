package cn.opensrcdevelop.test.controller;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/test")
public class TestController {

    @GetMapping
    @PreAuthorize("@pms.hasAnyPermission('createTest')")
    public String hello() {
        return "hello";
    }
}
