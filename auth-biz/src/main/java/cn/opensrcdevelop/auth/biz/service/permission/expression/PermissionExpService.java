package cn.opensrcdevelop.auth.biz.service.permission.expression;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.DebugPermissionExpRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.DebugPermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.auth.biz.entity.permission.PermissionExp;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface PermissionExpService extends IService<PermissionExp> {

    void createPermissionExp(PermissionExpRequestDto requestDto);

    PageData<PermissionExpResponseDto> list(int page, int size, String keyword);

    PermissionExpResponseDto detail(String permissionExpId);

    void updatePermissionExp(PermissionExpRequestDto requestDto);

    void removePermissionExp(String permissionExpId);

    DebugPermissionExpResponseDto debugPermissionExp(DebugPermissionExpRequestDto requestDto);

    List<PermissionResponseDto> expPermissions(String permissionExpId);
}
