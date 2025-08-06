package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.mapper.TableFieldMapper;
import cn.opensrcdevelop.ai.service.TableFieldService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class TableFieldServiceImpl extends ServiceImpl<TableFieldMapper, TableField> implements TableFieldService {
}
