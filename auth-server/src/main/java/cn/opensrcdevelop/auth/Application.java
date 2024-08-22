package cn.opensrcdevelop.auth;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages = "cn.opensrcdevelop")
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
