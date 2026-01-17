package cn.opensrcdevelop.auth.biz.service.user.excel.impl;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportErrorDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.UserExcelImportDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrMappingService;
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
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserExcelServiceImpl implements UserExcelService {

    private final UserService userService;
    private final UserAttrService userAttrService;
    private final UserAttrMappingService userAttrMappingService;
    private final DictDataService dictDataService;
    private final PasswordEncoder passwordEncoder;
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
        // 构建字典数据缓存
        Map<String, Map<String, String>> dictValueToIdMap = new HashMap<>();

        for (UserAttrResponseDto attr : allAttrs.getList()) {
            attrMap.put(attr.getKey(), attr);
            if (BooleanUtils.isTrue(attr.getExtFlg())) {
                extAttrs.add(attr);
                // 预加载字典数据
                if ("DICT".equals(attr.getDataType()) && attr.getDictId() != null) {
                    dictValueToIdMap.put(attr.getKey(), loadDictValueToIdMap(attr.getDictId()));
                }
            }
        }

        // 2. 读取 Excel 数据并收集所有用户名、邮箱、手机号
        Set<String> excelUsernames = new HashSet<>();
        Set<String> excelEmails = new HashSet<>();
        Set<String> excelPhones = new HashSet<>();

        try {
            EasyExcel.read(file.getInputStream(), UserExcelImportDto.class,
                    new PageReadListener<UserExcelImportDto>(dataList -> {
                        for (int i = 0; i < dataList.size(); i++) {
                            UserExcelImportDto dto = dataList.get(i);
                            int rowNum = i + 2; // Excel 行号（表头是第1行）

                            // 收集 Excel 中的用户名、邮箱、手机号用于重复性检查
                            collectExcelValues(dto, rowNum, excelUsernames, excelEmails, excelPhones, errors);

                            // 验证并收集数据（暂不检查数据库重复）
                            validateBasicRules(dto, rowNum, errors, validData, attrMap, dictValueToIdMap,
                                    excelUsernames, excelEmails, excelPhones);
                        }
                    }))
                    .sheet(0)
                    .doRead();
        } catch (IOException e) {
            log.error("读取 Excel 文件失败", e);
            throw new BizException("读取 Excel 文件失败", e);
        }

        // 3. 批量查询数据库中已存在的用户名、邮箱、手机号
        Set<String> existUsernames = batchQueryExistingUsers(excelUsernames, "username");
        Set<String> existEmails = batchQueryExistingUsers(excelEmails, "emailAddress");
        Set<String> existPhones = batchQueryExistingUsers(excelPhones, "phoneNumber");

        // 4. 检查新增用户是否与数据库重复
        List<ExcelImportErrorDto> dbDuplicateErrors = checkDbDuplicates(validData, existUsernames, existEmails,
                existPhones);
        errors.addAll(dbDuplicateErrors);

        // 5. 如果有错误，不执行数据库操作
        if (!errors.isEmpty()) {
            return ExcelImportResultDto.builder()
                    .successCount(0)
                    .failureCount(errors.size())
                    .errors(errors)
                    .build();
        }

        // 6. 批量执行数据库操作（只有所有数据校验通过才会执行到这里）
        int successCount = batchExecuteDatabaseOperations(validData, extAttrs, attrMap, errors);

        return ExcelImportResultDto.builder()
                .successCount(successCount)
                .failureCount(errors.size())
                .errors(errors)
                .build();
    }

    /**
     * 批量执行数据库操作
     */
    private int batchExecuteDatabaseOperations(List<UserExcelImportDto> validData,
            List<UserAttrResponseDto> extAttrs, Map<String, UserAttrResponseDto> attrMap,
            List<ExcelImportErrorDto> errors) {
        int successCount = 0;

        // 1. 收集不同操作类型的 DTO
        List<UserExcelImportDto> createDtos = new ArrayList<>();
        List<UserExcelImportDto> updateDtos = new ArrayList<>();
        List<UserExcelImportDto> deleteDtos = new ArrayList<>();

        for (UserExcelImportDto dto : validData) {
            if (dto.getOperationType() == 0) {
                createDtos.add(dto);
            } else if (dto.getOperationType() == 1) {
                updateDtos.add(dto);
            } else if (dto.getOperationType() == 2) {
                deleteDtos.add(dto);
            }
        }

        // 2. 收集待更新的用户名，用于批量查询用户ID
        List<String> updateUsernames = updateDtos.stream()
                .map(UserExcelImportDto::getUsername)
                .toList();
        List<String> deleteUsernames = deleteDtos.stream()
                .map(UserExcelImportDto::getUsername)
                .toList();

        // 批量查询用户名对应的用户ID
        Map<String, String> usernameToUserId = new HashMap<>();
        if (!updateUsernames.isEmpty() || !deleteUsernames.isEmpty()) {
            List<String> allQueryUsernames = new ArrayList<>(updateUsernames);
            allQueryUsernames.addAll(deleteUsernames);
            List<User> existingUsers = userService.lambdaQuery()
                    .in(User::getUsername, allQueryUsernames)
                    .list();
            for (User user : existingUsers) {
                usernameToUserId.put(user.getUsername(), user.getUserId());
            }
        }

        // 检查更新/删除的用户是否存在
        for (UserExcelImportDto dto : updateDtos) {
            if (!usernameToUserId.containsKey(dto.getUsername())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(validData.indexOf(dto) + 2)
                        .column("username")
                        .message("用户不存在: " + dto.getUsername())
                        .build());
            }
        }
        for (UserExcelImportDto dto : deleteDtos) {
            if (!usernameToUserId.containsKey(dto.getUsername())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(validData.indexOf(dto) + 2)
                        .column("username")
                        .message("用户不存在: " + dto.getUsername())
                        .build());
            }
        }

        // 如果有错误，不执行数据库操作
        if (!errors.isEmpty()) {
            return 0;
        }

        // 3. 批量创建用户
        List<User> createUsers = new ArrayList<>();
        List<UserAttrMapping> createAttrMappings = new ArrayList<>();
        for (UserExcelImportDto dto : createDtos) {
            User user = buildUserFromExcel(dto, extAttrs, null);
            createUsers.add(user);
        }

        try {
            // 批量保存用户
            if (!createUsers.isEmpty()) {
                userService.saveBatch(createUsers);
            }

            // 批量查询新创建用户的 ID
            if (!createDtos.isEmpty()) {
                List<String> createdUsernames = createDtos.stream()
                        .map(UserExcelImportDto::getUsername)
                        .toList();
                List<User> createdUsers = userService.lambdaQuery()
                        .in(User::getUsername, createdUsernames)
                        .list();
                for (User user : createdUsers) {
                    usernameToUserId.put(user.getUsername(), user.getUserId());
                }

                // 构建新用户的扩展属性映射
                for (UserExcelImportDto dto : createDtos) {
                    String userId = usernameToUserId.get(dto.getUsername());
                    if (userId != null) {
                        List<UserAttrMapping> mappings = buildAttrMappingsFromExcel(dto, extAttrs, userId);
                        createAttrMappings.addAll(mappings);
                    }
                }

                // 批量保存扩展属性
                if (!createAttrMappings.isEmpty()) {
                    userAttrMappingService.saveBatch(createAttrMappings);
                }
            }

            // 4. 批量更新用户
            List<User> updateUsers = new ArrayList<>();
            // 收集更新用户的扩展属性映射
            List<UserAttrMapping> updateAttrMappings = new ArrayList<>();

            // 批量查询所有需要更新用户的现有扩展属性
            Set<String> allUserIdsForUpdate = updateDtos.stream()
                    .map(dto -> usernameToUserId.get(dto.getUsername()))
                    .filter(id -> id != null)
                    .collect(java.util.stream.Collectors.toSet());

            // 查询现有映射
            Map<String, Map<String, UserAttrMapping>> existingMappings = new HashMap<>();
            if (!allUserIdsForUpdate.isEmpty()) {
                List<UserAttrMapping> existingList = userAttrMappingService.list(
                        new LambdaQueryWrapper<UserAttrMapping>()
                                .in(UserAttrMapping::getUserId, allUserIdsForUpdate));
                for (UserAttrMapping mapping : existingList) {
                    existingMappings
                            .computeIfAbsent(mapping.getUserId(), k -> new HashMap<>())
                            .put(mapping.getAttrId(), mapping);
                }
            }

            // 构建 attrKey -> attrId 的映射
            Map<String, String> attrKeyToId = new HashMap<>();
            for (UserAttrResponseDto attr : extAttrs) {
                attrKeyToId.put(attr.getKey(), attr.getId());
            }

            // 处理每个更新用户的扩展属性
            for (UserExcelImportDto dto : updateDtos) {
                String userId = usernameToUserId.get(dto.getUsername());
                if (userId == null) {
                    continue;
                }

                User user = buildUserFromExcel(dto, extAttrs, userId);
                updateUsers.add(user);

                // 处理扩展属性：只更新 Excel 中有值的属性
                Map<String, Object> extAttrMap = dto.getExtAttrs();
                if (extAttrMap != null) {
                    Map<String, UserAttrMapping> userExistingMappings = existingMappings.getOrDefault(userId,
                            new HashMap<>());

                    for (Map.Entry<String, Object> entry : extAttrMap.entrySet()) {
                        String attrKey = entry.getKey();
                        Object value = entry.getValue();

                        // 跳过空值
                        if (value == null || StringUtils.isBlank(String.valueOf(value))) {
                            continue;
                        }

                        String attrId = attrKeyToId.get(attrKey);
                        if (attrId == null) {
                            continue;
                        }

                        String attrValue = String.valueOf(value);

                        // 处理字典类型 - 存储字典数据ID
                        UserAttrResponseDto attr = attrMap.get(attrKey);
                        if (attr != null && "DICT".equals(attr.getDataType())
                                && StringUtils.isNotBlank(attr.getDictId())) {
                            String dictDataId = findDictDataIdByLabel(attr.getDictId(), attrValue);
                            if (dictDataId != null) {
                                attrValue = dictDataId;
                            }
                        }

                        // 处理日期类型 - 转换为时间戳
                        if (attr != null) {
                            if ("DATE".equals(attr.getDataType())) {
                                attrValue = convertDateToTimestamp(attrValue);
                            } else if ("DATETIME".equals(attr.getDataType())) {
                                attrValue = convertDateTimeToTimestamp(attrValue);
                            }
                        }

                        // 检查是否已存在该映射
                        UserAttrMapping existingMapping = userExistingMappings.get(attrId);
                        if (existingMapping != null) {
                            // 更新现有映射的值
                            existingMapping.setAttrValue(attrValue);
                            updateAttrMappings.add(existingMapping);
                        } else {
                            // 创建新映射
                            UserAttrMapping newMapping = new UserAttrMapping();
                            newMapping.setUserId(userId);
                            newMapping.setAttrId(attrId);
                            newMapping.setAttrValue(attrValue);
                            updateAttrMappings.add(newMapping);
                        }
                    }
                }
            }

            if (!updateUsers.isEmpty()) {
                userService.updateBatchById(updateUsers);
            }
            if (!updateAttrMappings.isEmpty()) {
                userAttrMappingService.saveOrUpdateBatch(updateAttrMappings);
            }

            // 5. 批量删除用户
            List<String> deleteUserIds = new ArrayList<>();
            for (UserExcelImportDto dto : deleteDtos) {
                String userId = usernameToUserId.get(dto.getUsername());
                if (userId != null) {
                    deleteUserIds.add(userId);
                }
            }

            if (!deleteUserIds.isEmpty()) {
                // 先删除扩展属性
                userAttrMappingService.remove(
                        new LambdaQueryWrapper<UserAttrMapping>()
                                .in(UserAttrMapping::getUserId, deleteUserIds));
                // 再删除用户
                userService.removeBatchByIds(deleteUserIds);
            }

            successCount = createDtos.size() + updateDtos.size() + deleteDtos.size();

        } catch (Exception e) {
            log.error("批量数据库操作失败", e);
            for (UserExcelImportDto dto : validData) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(validData.indexOf(dto) + 2)
                        .column("数据处理")
                        .message("批量操作失败: " + e.getMessage())
                        .build());
            }
            return 0;
        }

        return successCount;
    }

    /**
     * 从 Excel 构建用户实体（基础字段）
     */
    private User buildUserFromExcel(UserExcelImportDto dto, List<UserAttrResponseDto> extAttrs, String userId) {
        User user = new User();
        if (userId != null) {
            user.setUserId(userId);
        }
        user.setUsername(dto.getUsername());
        // 只有创建用户时才生成随机密码并加密
        if (dto.getOperationType() == 0) {
            user.setPassword(passwordEncoder.encode(CommonUtil.generateRandomString(12)));
            user.setNeedChangePwd(true);
        }
        user.setEmailAddress(dto.getEmailAddress());
        user.setPhoneNumber(dto.getPhoneNumber());
        user.setConsoleAccess(dto.getConsoleAccess());
        user.setEnableMfa(dto.getEnableMfa());
        return user;
    }

    /**
     * 从 Excel 构建扩展属性映射列表
     */
    private List<UserAttrMapping> buildAttrMappingsFromExcel(UserExcelImportDto dto,
            List<UserAttrResponseDto> extAttrs, String userId) {
        List<UserAttrMapping> mappings = new ArrayList<>();
        Map<String, Object> extAttrMap = dto.getExtAttrs();

        if (extAttrMap == null) {
            return mappings;
        }

        for (UserAttrResponseDto attr : extAttrs) {
            Object value = extAttrMap.get(attr.getKey());
            if (value == null || StringUtils.isBlank(String.valueOf(value))) {
                continue;
            }

            String attrValue = String.valueOf(value);

            // 处理字典类型 - 存储字典数据ID
            if ("DICT".equals(attr.getDataType()) && StringUtils.isNotBlank(attr.getDictId())) {
                String dictDataId = findDictDataIdByLabel(attr.getDictId(), attrValue);
                if (dictDataId != null) {
                    attrValue = dictDataId;
                }
            }

            // 处理日期类型 - 转换为时间戳
            if ("DATE".equals(attr.getDataType())) {
                attrValue = convertDateToTimestamp(attrValue);
            } else if ("DATETIME".equals(attr.getDataType())) {
                attrValue = convertDateTimeToTimestamp(attrValue);
            }

            UserAttrMapping mapping = new UserAttrMapping();
            mapping.setUserId(userId);
            mapping.setAttrId(attr.getId());
            mapping.setAttrValue(attrValue);
            mappings.add(mapping);
        }

        return mappings;
    }

    /**
     * 批量查询已存在的用户值
     */
    private Set<String> batchQueryExistingUsers(Set<String> values, String fieldName) {
        Set<String> existingValues = new HashSet<>();
        if (values == null || values.isEmpty()) {
            return existingValues;
        }

        // 批量查询（使用 IN 查询）
        List<User> users = userService.lambdaQuery()
                .in(fieldName.equals("username"), User::getUsername, values)
                .in(fieldName.equals("emailAddress"), User::getEmailAddress, values)
                .in(fieldName.equals("phoneNumber"), User::getPhoneNumber, values)
                .list();

        for (User user : users) {
            switch (fieldName) {
                case "username" :
                    if (user.getUsername() != null) {
                        existingValues.add(user.getUsername());
                    }
                    break;
                case "emailAddress" :
                    if (user.getEmailAddress() != null) {
                        existingValues.add(user.getEmailAddress());
                    }
                    break;
                case "phoneNumber" :
                    if (user.getPhoneNumber() != null) {
                        existingValues.add(user.getPhoneNumber());
                    }
                    break;
                default :
                    break;
            }
        }

        return existingValues;
    }

    /**
     * 检查数据库重复
     */
    private List<ExcelImportErrorDto> checkDbDuplicates(List<UserExcelImportDto> validData,
            Set<String> existUsernames, Set<String> existEmails, Set<String> existPhones) {
        List<ExcelImportErrorDto> errors = new ArrayList<>();

        for (int i = 0; i < validData.size(); i++) {
            UserExcelImportDto dto = validData.get(i);
            int rowNum = i + 2;

            if (dto.getOperationType() == null || dto.getOperationType() != 0) {
                continue; // 只检查新增操作
            }

            // 检查用户名
            if (StringUtils.isNotBlank(dto.getUsername()) && existUsernames.contains(dto.getUsername())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("username")
                        .message("用户名已存在: " + dto.getUsername())
                        .build());
            }

            // 检查邮箱
            if (StringUtils.isNotBlank(dto.getEmailAddress()) && existEmails.contains(dto.getEmailAddress())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("emailAddress")
                        .message("邮箱已被注册: " + dto.getEmailAddress())
                        .build());
            }

            // 检查手机号
            if (StringUtils.isNotBlank(dto.getPhoneNumber()) && existPhones.contains(dto.getPhoneNumber())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("phoneNumber")
                        .message("手机号已被注册: " + dto.getPhoneNumber())
                        .build());
            }
        }

        return errors;
    }

    /**
     * 收集 Excel 中的用户名、邮箱、手机号
     */
    private void collectExcelValues(UserExcelImportDto dto, int rowNum,
            Set<String> excelUsernames, Set<String> excelEmails, Set<String> excelPhones,
            List<ExcelImportErrorDto> errors) {
        // 收集用户名
        if (StringUtils.isNotBlank(dto.getUsername())) {
            if (!excelUsernames.add(dto.getUsername())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("username")
                        .message("用户名在 Excel 中重复")
                        .build());
            }
        }

        // 收集邮箱
        if (StringUtils.isNotBlank(dto.getEmailAddress())) {
            if (!excelEmails.add(dto.getEmailAddress())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("emailAddress")
                        .message("邮箱在 Excel 中重复")
                        .build());
            }
        }

        // 收集手机号
        if (StringUtils.isNotBlank(dto.getPhoneNumber())) {
            if (!excelPhones.add(dto.getPhoneNumber())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column("phoneNumber")
                        .message("手机号在 Excel 中重复")
                        .build());
            }
        }
    }

    /**
     * 加载字典数据到值->ID 映射
     */
    private Map<String, String> loadDictValueToIdMap(String dictId) {
        Map<String, String> valueToIdMap = new HashMap<>();
        List<DictDataResponseDto> dictData = dictDataService.getEnabledDictData(dictId);
        List<DictTreeFlattener.DictDisplayItem> items = DictTreeFlattener.flattenTreeWithId(dictData);
        for (DictTreeFlattener.DictDisplayItem item : items) {
            valueToIdMap.put(item.displayText(), item.id());
        }
        return valueToIdMap;
    }

    /**
     * 验证基础规则（格式验证、必填验证等） 注意：数据库重复性检查在 batchQueryExistingUsers 和 checkDbDuplicates
     * 中批量进行
     */
    private void validateBasicRules(UserExcelImportDto dto, int rowNum, List<ExcelImportErrorDto> errors,
            List<UserExcelImportDto> validData, Map<String, UserAttrResponseDto> attrMap,
            Map<String, Map<String, String>> dictValueToIdMap,
            Set<String> excelUsernames, Set<String> excelEmails, Set<String> excelPhones) {
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
            if (attr != null && StringUtils.isNotBlank(String.valueOf(value))) {
                String valueStr = String.valueOf(value);
                String dataType = attr.getDataType();

                // 根据数据类型验证格式
                switch (dataType) {
                    case "NUMBER" :
                        try {
                            Double.parseDouble(valueStr);
                        } catch (NumberFormatException e) {
                            rowErrors.add("字段 [" + attr.getName() + "] 的值必须是数字");
                        }
                        break;
                    case "DATE" :
                        // 验证日期格式：yyyyMMdd（如：20260109）
                        if (!isValidDateFormat(valueStr)) {
                            rowErrors.add("字段 [" + attr.getName() + "] 的日期格式无效，正确格式：20260109");
                        }
                        break;
                    case "DATETIME" :
                        // 验证日期时间格式：yyyyMMddHHmmss（如：20260109163045）
                        if (!isValidDateTimeFormat(valueStr)) {
                            rowErrors.add("字段 [" + attr.getName() + "] 的日期时间格式无效，正确格式：20260109163045");
                        }
                        break;
                    case "BOOLEAN" :
                        // 验证布尔值：是/否
                        if (!"是".equals(valueStr) && !"否".equals(valueStr)) {
                            rowErrors.add("字段 [" + attr.getName() + "] 的值必须是 是 或 否");
                        }
                        break;
                    case "DICT" :
                        // 验证字典值是否在有效范围内
                        Map<String, String> valueToIdMap = dictValueToIdMap.get(key);
                        if (valueToIdMap != null && !valueToIdMap.containsKey(valueStr)) {
                            rowErrors.add("字段 [" + attr.getName() + "] 的值不在有效范围内");
                        }
                        break;
                    default :
                        break;
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
     * 验证日期格式 yyyyMMdd
     */
    private boolean isValidDateFormat(String value) {
        if (value == null || value.length() != 8) {
            return false;
        }
        try {
            Integer.parseInt(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * 验证日期时间格式 yyyyMMddHHmmss
     */
    private boolean isValidDateTimeFormat(String value) {
        if (value == null || value.length() != 14) {
            return false;
        }
        try {
            Long.parseLong(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * 根据标签查找字典数据ID
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

    /**
     * 将日期格式 yyyymmdd 转换为时间戳
     */
    private String convertDateToTimestamp(String dateStr) {
        try {
            LocalDate date = LocalDate.parse(dateStr, DateTimeFormatter.ofPattern("yyyyMMdd"));
            return String.valueOf(date.atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli());
        } catch (DateTimeParseException e) {
            log.warn("日期转换失败: {}", dateStr);
            return dateStr;
        }
    }

    /**
     * 将日期时间格式 yyyymmddhhmmss 转换为时间戳
     */
    private String convertDateTimeToTimestamp(String dateTimeStr) {
        try {
            LocalDateTime dateTime = LocalDateTime.parse(dateTimeStr,
                    DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
            return String.valueOf(dateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
        } catch (DateTimeParseException e) {
            log.warn("日期时间转换失败: {}", dateTimeStr);
            return dateTimeStr;
        }
    }
}
