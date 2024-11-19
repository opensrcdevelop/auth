package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.DictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.DictResponseDto;
import cn.opensrcdevelop.auth.biz.entity.Dict;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface DictService extends IService<Dict> {

    void createDict(DictRequestDto requestDto);

    void updateDict(DictRequestDto requestDto);

    DictResponseDto detail(String dictId);

    PageData<DictResponseDto> list(int page, int size, String keyword);

    void removeDict(String dictId);
}
