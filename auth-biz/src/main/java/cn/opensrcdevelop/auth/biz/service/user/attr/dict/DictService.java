package cn.opensrcdevelop.auth.biz.service.user.attr.dict;

import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.ChildDictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.Dict;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface DictService extends IService<Dict> {

    void createDict(DictRequestDto requestDto);

    void updateDict(DictRequestDto requestDto);

    DictResponseDto detail(String dictId);

    PageData<DictResponseDto> list(int page, int size, String keyword, boolean queryChildren);

    void removeDict(String dictId);

    List<DictResponseDto> listSelectableChildren(String dictId);

    void addChildDicts(List<ChildDictRequestDto> requestDto);

    void removeChildDict(ChildDictRequestDto requestDto);

    boolean hasChildDict(String dictId);
}
