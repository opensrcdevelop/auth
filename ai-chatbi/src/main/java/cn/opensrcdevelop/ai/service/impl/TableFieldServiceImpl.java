package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.BatchUpdateTableFieldRequestDto;
import cn.opensrcdevelop.ai.dto.TableFieldResponseDto;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.enums.TableFieldType;
import cn.opensrcdevelop.ai.mapper.TableFieldMapper;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;

@Service
public class TableFieldServiceImpl extends ServiceImpl<TableFieldMapper, TableField> implements TableFieldService {

    /**
     * 获取表字段列表
     *
     * @param tableId 表ID
     * @param keyword 字段名称检索关键字
     * @param page    页数
     * @param size    条数
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
                        .type(TableFieldType.valueOf(tableField.getFieldType()).getDisplayName())
                        .remark(tableField.getRemark())
                        .additionalInfo(tableField.getAdditionalInfo())
                        .toUse(tableField.getToUse()).build())
                .toList();
        pageData.setList(data);
        return pageData;
    }

    /**
     * 批量更新表字段
     *
     * @param requestDto 请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.CHAT_BI_DATA_SOURCE,
            sysOperation = SysOperationType.UPDATE,
            success = "批量更新了数据源中的表字段信息",
            fail = "批量更新数据源中的表字段信息失败"
    )
    @Override
    public void batchUpdate(BatchUpdateTableFieldRequestDto requestDto) {
        // 1. 属性编辑
        List<TableField> updateList = CommonUtil.stream(requestDto.getList()).map(tableField -> {
            TableField updateTableField = new TableField();
            updateTableField.setFieldId(tableField.getId());
            updateTableField.setRemark(tableField.getRemark());
            updateTableField.setAdditionalInfo(tableField.getAdditionalInfo());
            CommonUtil.callSetWithCheck(Objects::nonNull, updateTableField::setToUse, tableField::getToUse);

            // 审计比较对象
            AuditContext.addCompareObj(CompareObj.builder()
                    .id(tableField.getId())
                    .before(super.getById(tableField.getId()))
                    .after(updateTableField)
                    .build());

            return updateTableField;
        }).toList();

        // 2. 批量更新
        if (CollectionUtils.isNotEmpty(updateList)) {
            super.updateBatchById(updateList);
        }
    }

    /**
     * 删除表中的所有字段
     *
     * @param tableIds 表ID集合
     */
    @Transactional
    @Override
    public void removeTableFields(List<String> tableIds) {
        if (CollectionUtils.isNotEmpty(tableIds)) {
            super.remove(Wrappers.<TableField>lambdaQuery().in(TableField::getTableId, tableIds));
        }
    }
}
