package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.entity.PermissionExp;
import cn.opensrcdevelop.auth.biz.dto.PermissionExpRequestDto;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface PermissionExpService extends IService<PermissionExp> {

    void createPermissionExp(PermissionExpRequestDto requestDto);

    PageData<PermissionExpResponseDto> list(int page, int size, String keyword);

    PermissionExpResponseDto detail(String expressionId);

    void updatePermissionExp(PermissionExpRequestDto requestDto);

    void removePermissionExp(String permissionExpId);
}
