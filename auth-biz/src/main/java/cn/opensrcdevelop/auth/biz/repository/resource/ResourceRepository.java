package cn.opensrcdevelop.auth.biz.repository.resource;

import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import com.baomidou.mybatisplus.core.metadata.IPage;

public interface ResourceRepository {

    void searchResources(IPage<Resource> page, String keyword);
}
