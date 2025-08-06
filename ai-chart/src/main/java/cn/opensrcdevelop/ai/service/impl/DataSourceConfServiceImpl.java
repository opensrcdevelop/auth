package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.mapper.DataSourceConfMapper;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class DataSourceConfServiceImpl extends ServiceImpl<DataSourceConfMapper, DataSourceConf> implements DataSourceConfService {
}
