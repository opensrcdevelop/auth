package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.TableFieldResponseDto;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface TableFieldService extends IService<TableField> {

    PageData<TableFieldResponseDto> list(String tableId, String keyword, int page, int size);
}
