package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.ChartConf;
import cn.opensrcdevelop.ai.mapper.ChartConfMapper;
import cn.opensrcdevelop.ai.service.ChartConfService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class ChartConfServiceImpl extends ServiceImpl<ChartConfMapper, ChartConf> implements ChartConfService {
}
