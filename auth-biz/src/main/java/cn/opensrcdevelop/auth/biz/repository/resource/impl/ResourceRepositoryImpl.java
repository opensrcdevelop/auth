package cn.opensrcdevelop.auth.biz.repository.resource.impl;

import cn.opensrcdevelop.auth.biz.entity.resource.Resource;
import cn.opensrcdevelop.auth.biz.mapper.resource.ResourceMapper;
import cn.opensrcdevelop.auth.biz.repository.resource.ResourceRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class ResourceRepositoryImpl implements ResourceRepository {

    private final ResourceMapper resourceMapper;

    /**
     * 检索资源
     *
     * @param page
     *            分页对象
     * @param keyword
     *            资源名称 / 标识检索关键字
     */
    @Override
    public void searchResources(IPage<Resource> page, String keyword) {
        resourceMapper.searchResources(page, keyword);
    }
}
