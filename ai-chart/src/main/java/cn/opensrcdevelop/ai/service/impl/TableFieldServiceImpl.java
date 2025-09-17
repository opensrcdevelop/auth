package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.TableFieldResponseDto;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.mapper.TableFieldMapper;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class TableFieldServiceImpl extends ServiceImpl<TableFieldMapper, TableField> implements TableFieldService {

    /**
     * 获取表字段列表
     *
     * @param tableId 表ID
     * @param keyword 字段名称检索关键字
     * @param page 页数
     * @param size 条数
     * @return 表字段列表
     */
    @Override
    public PageData<TableFieldResponseDto> list(String tableId, String keyword, int page, int size) {
        // 1. 查询数据库
        List<TableField> tableFieldList;
        Page<TableField> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            tableFieldList = super.list(pageRequest, Wrappers.<TableField>lambdaQuery()
                    .eq(TableField::getTableId, tableId)
                    .like(TableField::getFieldName, keyword)
                    .orderByAsc(TableField::getFieldName));
        } else {
            tableFieldList = super.list(pageRequest, Wrappers.<TableField>lambdaQuery()
                    .eq(TableField::getTableId, tableId)
                    .orderByAsc(TableField::getFieldName));
        }

        // 2. 属性编辑
        PageData<TableFieldResponseDto> pageData = new PageData<>();
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());

        List<TableFieldResponseDto> data = CommonUtil.stream(tableFieldList).map(tableField -> TableFieldResponseDto.builder()
                .id(tableField.getFieldId())
                .name(tableField.getFieldName())
                .type(tableField.getFieldType())
                .remark(tableField.getRemark())
                .additionalInfo(tableField.getAdditionalInfo())
                .toUse(tableField.getToUse()).build())
                .toList();
        pageData.setList(data);
        return pageData;
    }
}
