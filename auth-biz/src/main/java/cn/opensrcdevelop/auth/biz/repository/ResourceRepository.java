package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.Resource;
import com.baomidou.mybatisplus.core.metadata.IPage;

public interface ResourceRepository {

    void searchResources(IPage<Resource> page, String keyword);
}
