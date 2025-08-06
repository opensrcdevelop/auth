package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.mapper.TableMapper;
import cn.opensrcdevelop.ai.service.TableService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class TableServiceImpl extends ServiceImpl<TableMapper, Table> implements TableService {
}
