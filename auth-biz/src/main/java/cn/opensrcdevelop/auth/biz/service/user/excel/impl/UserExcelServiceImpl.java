package cn.opensrcdevelop.auth.biz.service.user.excel.impl;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.UserRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportErrorDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.UserExcelImportDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.excel.UserExcelService;
import cn.opensrcdevelop.auth.biz.util.excel.DictTreeFlattener;
import cn.opensrcdevelop.auth.biz.util.excel.ExcelTemplateGenerator;
import cn.opensrcdevelop.auth.biz.util.excel.UserExcelExporter;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.read.listener.PageReadListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserExcelServiceImpl implements UserExcelService {

    private final UserService userService;
    private final UserAttrService userAttrService;
    private final DictDataService dictDataService;
    private final ExcelTemplateGenerator templateGenerator;
    private final UserExcelExporter userExcelExporter;

    @Override
    public byte[] generateImportTemplate() {
        // 获取所有用户字段（包括基础字段和扩展字段）
        List<UserAttrResponseDto> allFields = userAttrService.getAllUserAttrsForExcel();
        // 导入模版不需要 createTime 字段（创建时自动生成）
        List<UserAttrResponseDto> templateFields = allFields.stream()
                .filter(f -> !"createTime".equals(f.getKey()) && !"locked".equals(f.getKey()))
                .toList();
        return templateGenerator.generateTemplate(templateFields);
    }

    @Override
    public byte[] exportUsers(List<DataFilterDto> filters, boolean exportAll) {
        // 1. 获取所有用户字段（包括基础字段和扩展字段）
        List<UserAttrResponseDto> allFields = userAttrService.getAllUserAttrsForExcel();

        // 2. 获取用户数据（处理 filters 为 null 的情况）
        int size = exportAll ? Integer.MAX_VALUE : 100;
        List<DataFilterDto> safeFilters = filters != null ? filters : new ArrayList<>();
        PageData<Map<String, Object>> users = userService.list(1, size, safeFilters);

        // 安全获取用户列表，防止空指针
        List<Map<String, Object>> userList = users.getList() != null ? users.getList() : new ArrayList<>();

        // 3. 使用新的导出器生成 Excel
        return userExcelExporter.exportUsers(userList, allFields, filters);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ExcelImportResultDto importUsers(MultipartFile file) {
        List<ExcelImportErrorDto> errors = new ArrayList<>();
        List<UserExcelImportDto> validData = new ArrayList<>();

        // 1. 获取用户属性定义
        PageData<UserAttrResponseDto> allAttrs = userAttrService.listUserAttrs(1, Integer.MAX_VALUE, false, null);
        Map<String, UserAttrResponseDto> attrMap = new HashMap<>();
        List<UserAttrResponseDto> extAttrs = new ArrayList<>();
        for (UserAttrResponseDto attr : allAttrs.getList()) {
            attrMap.put(attr.getKey(), attr);
            if (BooleanUtils.isTrue(attr.getExtFlg())) {
                extAttrs.add(attr);
            }
        }

        // 2. 读取 Excel 数据
        try {
            EasyExcel.read(file.getInputStream(), UserExcelImportDto.class,
                    new PageReadListener<UserExcelImportDto>(dataList -> {
                        for (int i = 0; i < dataList.size(); i++) {
                            UserExcelImportDto dto = dataList.get(i);
                            int rowNum = i + 2; // Excel 行号（表头是第1行）
                            validateAndCollect(dto, rowNum, errors, validData, attrMap);
                        }
                    }))
                    .sheet(0)
                    .doRead();
        } catch (IOException e) {
            log.error("读取 Excel 文件失败", e);
            throw new BizException("读取 Excel 文件失败", e);
        }

        // 3. 执行数据库操作
        int successCount = 0;
        for (UserExcelImportDto dto : validData) {
            try {
                processImportRow(dto, extAttrs);
                successCount++;
            } catch (Exception e) {
                log.error("处理导入行失败: {}", dto, e);
                errors.add(ExcelImportErrorDto.builder()
                        .row(validData.indexOf(dto) + 2)
                        .column("数据处理")
                        .message("处理失败: " + e.getMessage())
                        .build());
            }
        }

        return ExcelImportResultDto.builder()
                .successCount(successCount)
                .failureCount(errors.size())
                .errors(errors)
                .build();
    }

    /**
     * 验证并收集数据
     */
    private void validateAndCollect(UserExcelImportDto dto, int rowNum, List<ExcelImportErrorDto> errors,
            List<UserExcelImportDto> validData, Map<String, UserAttrResponseDto> attrMap) {
        List<String> rowErrors = new ArrayList<>();

        // 验证操作类型
        if (dto.getOperationType() == null) {
            rowErrors.add("操作类型不能为空");
        } else if (dto.getOperationType() < 0 || dto.getOperationType() > 2) {
            rowErrors.add("操作类型无效（0:添加，1:更新，2:删除）");
        }

        // 根据操作类型验证必填字段
        if (dto.getOperationType() != null) {
            switch (dto.getOperationType()) {
                case 0 : // 添加
                    if (StringUtils.isBlank(dto.getUsername())) {
                        rowErrors.add("用户名不能为空");
                    }
                    if (StringUtils.isBlank(dto.getEmailAddress())) {
                        rowErrors.add("邮箱不能为空");
                    }
                    break;
                case 1 : // 更新
                case 2 : // 删除
                    if (StringUtils.isBlank(dto.getUsername())) {
                        rowErrors.add("用户名不能为空");
                    }
                    break;
            }
        }

        // 验证扩展字段
        for (Map.Entry<String, Object> entry : dto.getExtAttrs().entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            UserAttrResponseDto attr = attrMap.get(key);
            if (attr != null) {
                // 验证必填
                if (StringUtils.isNotBlank(String.valueOf(value))) {
                    // 根据数据类型验证格式
                    if ("NUMBER".equals(attr.getDataType())) {
                        try {
                            Double.parseDouble(String.valueOf(value));
                        } catch (NumberFormatException e) {
                            rowErrors.add("字段 [" + attr.getName() + "] 的值必须是数字");
                        }
                    } else if ("DICT".equals(attr.getDataType())) {
                        // 字典类型不在这里验证，在处理时转换
                    }
                }
            }
        }

        if (rowErrors.isEmpty()) {
            validData.add(dto);
        } else {
            for (String error : rowErrors) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("验证")
                        .message(error)
                        .build());
            }
        }
    }

    /**
     * 处理导入行
     */
    private void processImportRow(UserExcelImportDto dto, List<UserAttrResponseDto> extAttrs) {
        switch (dto.getOperationType()) {
            case 0 : // 添加用户
                createUserFromExcel(dto, extAttrs);
                break;
            case 1 : // 更新用户
                updateUserFromExcel(dto, extAttrs);
                break;
            case 2 : // 删除用户
                deleteUserFromExcel(dto);
                break;
        }
    }

    /**
     * 从 Excel 创建用户
     */
    private void createUserFromExcel(UserExcelImportDto dto, List<UserAttrResponseDto> extAttrs) {
        User user = getUserByUsername(dto.getUsername());
        if (user != null) {
            throw new BizException("用户名已存在: " + dto.getUsername());
        }

        // 创建用户请求
        UserRequestDto requestDto = new UserRequestDto();
        requestDto.setUsername(dto.getUsername());
        // 生成 12 位随机密码，包含大小写字母和数字
        requestDto.setPassword(CommonUtil.generateRandomString(12));
        requestDto.setEmailAddress(dto.getEmailAddress());
        requestDto.setPhoneNumber(dto.getPhoneNumber());
        requestDto.setLocked(dto.getLocked());
        requestDto.setConsoleAccess(dto.getConsoleAccess());
        requestDto.setEnableMfa(dto.getEnableMfa());
        requestDto.setNeedChangePwd(true); // 强制修改密码
        requestDto.setSendEmail(true); // 发送邮件通知

        // 设置扩展属性
        List<UserAttrMappingRequestDto> attributes = buildAttrMappings(dto, extAttrs);
        requestDto.setAttributes(attributes);

        userService.createUser(requestDto);
    }

    /**
     * 从 Excel 更新用户
     */
    private void updateUserFromExcel(UserExcelImportDto dto, List<UserAttrResponseDto> extAttrs) {
        User user = getUserByUsername(dto.getUsername());
        if (user == null) {
            throw new BizException("用户不存在: " + dto.getUsername());
        }

        // 更新用户请求
        UserRequestDto requestDto = new UserRequestDto();
        requestDto.setUserId(user.getUserId());
        requestDto.setUsername(dto.getUsername());
        requestDto.setEmailAddress(dto.getEmailAddress());
        requestDto.setPhoneNumber(dto.getPhoneNumber());
        requestDto.setLocked(dto.getLocked());
        requestDto.setConsoleAccess(dto.getConsoleAccess());
        requestDto.setEnableMfa(dto.getEnableMfa());

        // 设置扩展属性
        List<UserAttrMappingRequestDto> attributes = buildAttrMappings(dto, extAttrs);
        requestDto.setAttributes(attributes);

        userService.updateUser(requestDto);
    }

    /**
     * 从 Excel 删除用户
     */
    private void deleteUserFromExcel(UserExcelImportDto dto) {
        User user = getUserByUsername(dto.getUsername());
        if (user == null) {
            throw new BizException("用户不存在: " + dto.getUsername());
        }
        userService.removeUser(user.getUserId());
    }

    /**
     * 根据用户名获取用户
     */
    private User getUserByUsername(String username) {
        return userService.lambdaQuery()
                .eq(User::getUsername, username)
                .one();
    }

    /**
     * 构建扩展属性映射列表
     */
    private List<UserAttrMappingRequestDto> buildAttrMappings(UserExcelImportDto dto,
            List<UserAttrResponseDto> extAttrs) {
        List<UserAttrMappingRequestDto> attributes = new ArrayList<>();

        for (UserAttrResponseDto attr : extAttrs) {
            Object value = dto.getExtAttrs().get(attr.getKey());
            if (value != null && StringUtils.isNotBlank(String.valueOf(value))) {
                String attrValue = String.valueOf(value);

                // 处理字典类型
                if ("DICT".equals(attr.getDataType()) && StringUtils.isNotBlank(attr.getDictId())) {
                    // 查找字典数据ID
                    String dictDataId = findDictDataIdByLabel(attr.getDictId(), attrValue);
                    if (dictDataId != null) {
                        attrValue = dictDataId;
                    }
                }

                UserAttrMappingRequestDto mapping = new UserAttrMappingRequestDto();
                mapping.setAttrId(attr.getId());
                mapping.setAttrValue(attrValue);
                attributes.add(mapping);
            }
        }

        return attributes;
    }

    /**
     * 根据标签查找字典数据ID 优化版本：只调用一次 flattenTreeWithId，使用 Map 进行查找
     */
    private String findDictDataIdByLabel(String dictId, String label) {
        List<DictDataResponseDto> dictData = dictDataService.getEnabledDictData(dictId);
        List<DictTreeFlattener.DictDisplayItem> itemsWithId = DictTreeFlattener.flattenTreeWithId(dictData);

        // 构建 displayText -> id 的映射
        Map<String, String> displayTextToId = new HashMap<>();
        for (DictTreeFlattener.DictDisplayItem item : itemsWithId) {
            displayTextToId.put(item.displayText(), item.id());
        }

        // 查找匹配的显示文本
        for (Map.Entry<String, String> entry : displayTextToId.entrySet()) {
            if (entry.getKey().endsWith(label)) {
                return entry.getValue();
            }
        }
        return null;
    }
}
