package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.DataSourceConfResponseDto;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.mapper.DataSourceConfMapper;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class DataSourceConfServiceImpl extends ServiceImpl<DataSourceConfMapper, DataSourceConf> implements DataSourceConfService {

    /**
     * 获取数据源配置列表
     *
     * @param keyword 检索关键字
     * @param page 页数
     * @param size 条数
     * @return 数据源配置列表
     */
    @Override
    public PageData<DataSourceConfResponseDto> list(String keyword, int page, int size) {
        // 1. 查询数据库
        List<DataSourceConf> dataSourceConfList;
        Page<DataSourceConf> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            dataSourceConfList = super.list(pageRequest,  Wrappers.<DataSourceConf>lambdaQuery()
                    .select(DataSourceConf::getDataSourceId, DataSourceConf::getDataSourceName, DataSourceConf::getDataSourceType)
                    .like(DataSourceConf::getDataSourceName, keyword)
                    .orderByAsc(DataSourceConf::getDataSourceName)
            );
        } else {
            dataSourceConfList = super.list(pageRequest, Wrappers.<DataSourceConf>lambdaQuery()
                    .select(DataSourceConf::getDataSourceId, DataSourceConf::getDataSourceName, DataSourceConf::getDataSourceType)
                    .orderByAsc(DataSourceConf::getDataSourceName)
            );
        }

        // 2. 属性编辑
        PageData<DataSourceConfResponseDto> pageData = new PageData<>();
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());

        List<DataSourceConfResponseDto> data = CommonUtil.stream(dataSourceConfList).map(dataSourceConf -> DataSourceConfResponseDto.builder()
                .id(dataSourceConf.getDataSourceId())
                .name(dataSourceConf.getDataSourceName())
                .type(dataSourceConf.getDataSourceType()).build())
                .toList();
        pageData.setList(data);
        return pageData;
    }
}
