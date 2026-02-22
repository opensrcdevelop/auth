package cn.opensrcdevelop.auth.biz.service.asynctask.storage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 存储服务持有者，用于在非 Spring 管理的地方获取存储服务
 */
@Component
public class StorageServiceHolder {

    private static StorageService storageService;

    @Autowired
    public void setStorageService(StorageService storageService) {
        StorageServiceHolder.storageService = storageService;
    }

    public static StorageService getStorageService() {
        return storageService;
    }
}
