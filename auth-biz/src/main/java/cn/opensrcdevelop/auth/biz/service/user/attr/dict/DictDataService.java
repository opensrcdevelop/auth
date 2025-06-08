package cn.opensrcdevelop.auth.biz.service.user.attr.dict;

import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface DictDataService extends IService<DictData> {

    void createDictData(DictDataRequestDto requestDto);

    void updateDictData(DictDataRequestDto requestDto);

    DictDataResponseDto detail(String dictDataId);

    PageData<DictDataResponseDto> list(String dictId, int page, int size, String keyword);

    void removeDictData(List<String> dictDataIds);

    List<DictDataResponseDto> getEnabledDictData(String dictId);
}
