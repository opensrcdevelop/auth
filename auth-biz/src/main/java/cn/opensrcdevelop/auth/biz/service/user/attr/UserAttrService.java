package cn.opensrcdevelop.auth.biz.service.user.attr;

import cn.opensrcdevelop.auth.biz.dto.user.attr.SetUserAttrDisplaySeqRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
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

    List<UserAttrResponseDto> getVisibleUserAttrs();

    /**
     * 获取所有用户字段（包括基础字段和扩展字段） 基础字段从代码定义读取，扩展字段从数据库读取 用于 Excel 导入导出功能
     *
     * @return 所有用户字段列表
     */
    List<UserAttrResponseDto> getAllUserAttrsForExcel();
}
