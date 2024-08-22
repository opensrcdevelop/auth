package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.SetUserAttrDisplaySeqRequestDto;
import cn.opensrcdevelop.auth.biz.dto.UserAttrRequestDto;
import cn.opensrcdevelop.auth.biz.dto.UserAttrMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.entity.UserAttr;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface UserAttrService extends IService<UserAttr> {

    void createUserAttr(UserAttrRequestDto requestDto);

    void createUserAttrMapping(String userId, List<UserAttrMappingRequestDto> attributes);

    List<UserAttr> getUserAttrs(String userId);

    PageData<UserAttrResponseDto> listUserAttrs(int page, int size, Boolean onDisplay, String keyword);

    void updateUserUserAttrMapping(String userId, List<UserAttrMappingRequestDto> attributes);

    void updateUserAttr(UserAttrRequestDto requestDto);

    void setUserAttrDisplaySeq(List<SetUserAttrDisplaySeqRequestDto> requestDtoList);

    void removeUserAttrMapping(String userId);

    UserAttrResponseDto detail(String userAttrId);

    void removeUserAttr(String userAttrId);
}
