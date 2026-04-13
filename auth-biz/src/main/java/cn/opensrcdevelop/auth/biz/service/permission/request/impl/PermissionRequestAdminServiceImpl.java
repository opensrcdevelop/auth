package cn.opensrcdevelop.auth.biz.service.permission.request.impl;

import cn.opensrcdevelop.auth.biz.constants.PermissionRequestStatusEnum;
import cn.opensrcdevelop.auth.biz.dto.permission.request.PermissionRequestListItemDto;
import cn.opensrcdevelop.auth.biz.entity.permission.request.PermissionRequest;
import cn.opensrcdevelop.auth.biz.repository.permission.request.PermissionRequestRepository;
import cn.opensrcdevelop.auth.biz.service.permission.request.PermissionRequestAdminService;
import cn.opensrcdevelop.common.response.PageData;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PermissionRequestAdminServiceImpl implements PermissionRequestAdminService {

    private final PermissionRequestRepository permissionRequestRepository;

    @Override
    public PageData<PermissionRequestListItemDto> listPendingRequests(int page, int size) {
        List<String> statuses = List.of(
                PermissionRequestStatusEnum.PENDING.getCode(),
                PermissionRequestStatusEnum.AUTO_APPROVED.getCode());
        PageData<PermissionRequest> paged = permissionRequestRepository.findByStatuses(statuses, page, size);
        return toDtoList(paged);
    }

    @Override
    public PageData<PermissionRequestListItemDto> listAllRequests(int page, int size, String status) {
        PageData<PermissionRequest> paged;
        if (status != null) {
            paged = permissionRequestRepository.findByStatuses(List.of(status), page, size);
        } else {
            paged = permissionRequestRepository.findByTenantId(page, size);
        }
        return toDtoList(paged);
    }

    private PageData<PermissionRequestListItemDto> toDtoList(PageData<PermissionRequest> paged) {
        List<PermissionRequestListItemDto> dtoList = paged.getList().stream()
                .map(req -> {
                    PermissionRequestListItemDto dto = new PermissionRequestListItemDto();
                    dto.setRequestId(req.getRequestId());
                    dto.setStatus(req.getStatus());
                    dto.setRequestTime(req.getRequestTime());
                    dto.setReason(req.getReason());
                    dto.setRejectReason(req.getRejectReason());
                    return dto;
                })
                .toList();

        PageData<PermissionRequestListItemDto> result = new PageData<>();
        result.setTotal(paged.getTotal());
        result.setPages(paged.getPages());
        result.setCurrent(paged.getCurrent());
        result.setSize(paged.getSize());
        result.setList(dtoList);
        return result;
    }
}
