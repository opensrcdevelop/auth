package cn.opensrcdevelop.auth.biz.service.user.excel.impl;

import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportErrorDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.UserExcelImportDto;
import cn.opensrcdevelop.auth.biz.entity.role.RoleMapping;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.service.role.RoleMappingService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrMappingService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.excel.UserExcelService;
import cn.opensrcdevelop.auth.biz.service.user.group.UserGroupMappingService;
import cn.opensrcdevelop.auth.biz.util.excel.DictTreeFlattener;
import cn.opensrcdevelop.auth.biz.util.excel.ExcelTemplateGenerator;
import cn.opensrcdevelop.auth.biz.util.excel.UserExcelExporter;
import cn.opensrcdevelop.auth.biz.util.excel.UserExcelImportHandler;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
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
    private final RoleMappingService roleMappingService;
    private final UserGroupMappingService userGroupMappingService;
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
        Map<String, UserAttrResponseDto> attrMap = new LinkedHashMap<>();
        List<UserAttrResponseDto> extAttrs = new ArrayList<>();
        // 构建字典数据缓存
        Map<String, Map<String, String>> dictValueToIdMap = new LinkedHashMap<>();

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
        // 字段 key -> 中文标题的映射（用于错误显示）
        Map<String, String> keyToHeaderMap = new HashMap<>();

        // 读取文件内容到字节数组（避免多次调用 getInputStream）
        byte[] fileBytes;
        try {
            fileBytes = file.getBytes();
        } catch (IOException e) {
            log.error("读取上传文件失败", e);
            throw new BizException("读取上传文件失败", e);
        }
        if (fileBytes.length == 0) {
            throw new BizException("上传的文件为空");
        }

        // 使用字节数组创建 InputStream
        ByteArrayInputStream excelStream = new ByteArrayInputStream(fileBytes);
        try {
            // 使用自定义处理器读取 Excel（支持中文标题映射）
            UserExcelImportHandler handler = new UserExcelImportHandler(attrMap, dictValueToIdMap, excelStream);
            // 获取字段 key -> 中文标题的映射（用于错误显示）
            keyToHeaderMap = handler.getKeyToHeaderMap();

            // 重置流位置（因为 UserExcelImportHandler 读取后位置已移动）
            excelStream.reset();
            EasyExcel.read(excelStream)
                    .registerReadListener(handler)
                    .sheet(0)
                    .headRowNumber(2) // 前两行是标题
                    .doRead();

            // 获取读取的数据并处理
            List<UserExcelImportDto> allData = handler.getDataList();
            log.info("总共读取到 {} 条数据", allData.size());
            for (int i = 0; i < allData.size(); i++) {
                UserExcelImportDto dto = allData.get(i);
                int rowNum = i + 3; // Excel 行号（第1、2行是标题，第3行开始是数据）

                // 收集 Excel 中的用户名、邮箱、手机号用于重复性检查
                collectExcelValues(dto, rowNum, excelUsernames, excelEmails, excelPhones, errors, keyToHeaderMap);

                // 验证并收集数据（暂不检查数据库重复）
                validateBasicRules(dto, rowNum, errors, validData, attrMap, dictValueToIdMap,
                        excelUsernames, excelEmails, excelPhones, keyToHeaderMap);
            }
        } catch (Exception e) {
            log.error("读取 Excel 文件失败", e);
            throw new BizException("读取 Excel 文件失败", e);
        }

        // 3. 批量查询数据库中已存在的用户名、邮箱、手机号
        Set<String> existUsernames = batchQueryExistingUsers(excelUsernames, "username");
        Set<String> existEmails = batchQueryExistingUsers(excelEmails, "emailAddress");
        Set<String> existPhones = batchQueryExistingUsers(excelPhones, "phoneNumber");

        // 4. 检查新增用户是否与数据库重复
        List<ExcelImportErrorDto> dbDuplicateErrors = checkDbDuplicates(validData, existUsernames, existEmails,
                existPhones, keyToHeaderMap);
        errors.addAll(dbDuplicateErrors);

        // 5. 如果有错误，不执行数据库操作
        if (!errors.isEmpty()) {
            return ExcelImportResultDto.builder()
                    .createdCount(0)
                    .updatedCount(0)
                    .deletedCount(0)
                    .errors(errors)
                    .build();
        }

        // 6. 批量执行数据库操作（只有所有数据校验通过才会执行到这里）
        ImportStats stats = batchExecuteDatabaseOperations(validData, extAttrs, attrMap, errors, keyToHeaderMap);

        return ExcelImportResultDto.builder()
                .createdCount(stats.createdCount)
                .updatedCount(stats.updatedCount)
                .deletedCount(stats.deletedCount)
                .errors(errors)
                .build();
    }

    /**
     * 导入统计结果
     */
    private record ImportStats(int createdCount, int updatedCount, int deletedCount) {
    }

    /**
     * 批量执行数据库操作
     */
    private ImportStats batchExecuteDatabaseOperations(List<UserExcelImportDto> validData,
            List<UserAttrResponseDto> extAttrs, Map<String, UserAttrResponseDto> attrMap,
            List<ExcelImportErrorDto> errors, Map<String, String> keyToHeaderMap) {
        int createdCount = 0;
        int updatedCount = 0;
        int deletedCount = 0;

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

        // 2. 验证更新/删除用户的 userId 是否存在，并收集 userId
        // 同时验证 userId 和 username 是否匹配
        Map<String, String> usernameToUserId = new HashMap<>();
        List<String> allUserIds = new ArrayList<>();
        List<String> updateUsernames = new ArrayList<>();
        List<String> deleteUsernames = new ArrayList<>();

        for (UserExcelImportDto dto : updateDtos) {
            String userId = dto.getUserId();
            String username = dto.getUsername();
            if (StringUtils.isNotBlank(userId)) {
                allUserIds.add(userId);
                updateUsernames.add(username);
                usernameToUserId.put(username, userId);
            }
        }
        for (UserExcelImportDto dto : deleteDtos) {
            String userId = dto.getUserId();
            String username = dto.getUsername();
            if (StringUtils.isNotBlank(userId)) {
                allUserIds.add(userId);
                deleteUsernames.add(username);
                usernameToUserId.put(username, userId);
            }
        }

        // 批量查询用户，验证 userId 和 username 是否匹配
        if (!allUserIds.isEmpty()) {
            List<User> existingUsers = userService.lambdaQuery()
                    .in(User::getUserId, allUserIds)
                    .list();
            Map<String, User> userIdToUser = new HashMap<>();
            for (User user : existingUsers) {
                userIdToUser.put(user.getUserId(), user);
            }

            // 验证更新用户
            for (UserExcelImportDto dto : updateDtos) {
                String userId = dto.getUserId();
                if (StringUtils.isNotBlank(userId)) {
                    User user = userIdToUser.get(userId);
                    if (user == null) {
                        errors.add(ExcelImportErrorDto.builder()
                                .row(validData.indexOf(dto) + 2)
                                .column(keyToHeaderMap.getOrDefault("userId", "用户ID"))
                                .message("用户ID不存在: " + userId)
                                .build());
                    } else if (!user.getUsername().equals(dto.getUsername())) {
                        errors.add(ExcelImportErrorDto.builder()
                                .row(validData.indexOf(dto) + 2)
                                .column(keyToHeaderMap.getOrDefault("username", "用户名*"))
                                .message("用户名与用户ID不匹配")
                                .build());
                    }
                }
            }
            // 验证删除用户
            for (UserExcelImportDto dto : deleteDtos) {
                String userId = dto.getUserId();
                if (StringUtils.isNotBlank(userId)) {
                    User user = userIdToUser.get(userId);
                    if (user == null) {
                        errors.add(ExcelImportErrorDto.builder()
                                .row(validData.indexOf(dto) + 2)
                                .column(keyToHeaderMap.getOrDefault("userId", "用户ID"))
                                .message("用户ID不存在: " + userId)
                                .build());
                    } else if (!user.getUsername().equals(dto.getUsername())) {
                        errors.add(ExcelImportErrorDto.builder()
                                .row(validData.indexOf(dto) + 2)
                                .column(keyToHeaderMap.getOrDefault("username", "用户名*"))
                                .message("用户名与用户ID不匹配")
                                .build());
                    }
                }
            }
        }

        // 如果有错误，不执行数据库操作
        if (!errors.isEmpty()) {
            return new ImportStats(0, 0, 0);
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

            // 批量查询所有需要更新用户的现有扩展属性（直接使用 dto 中的 userId）
            Set<String> allUserIdsForUpdate = updateDtos.stream()
                    .map(UserExcelImportDto::getUserId)
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
                String userId = dto.getUserId();
                if (StringUtils.isBlank(userId)) {
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
            // 更新扩展属性：只删除要更新的属性ID对应的映射（不是删除用户的所有属性）
            if (!updateAttrMappings.isEmpty()) {
                // 收集需要更新的 (userId, attrId) 对，只删除这些特定的映射
                for (UserAttrMapping mapping : updateAttrMappings) {
                    userAttrMappingService.remove(
                            new LambdaQueryWrapper<UserAttrMapping>()
                                    .eq(UserAttrMapping::getUserId, mapping.getUserId())
                                    .eq(UserAttrMapping::getAttrId, mapping.getAttrId()));
                }
                // 批量插入新的属性映射
                userAttrMappingService.saveBatch(updateAttrMappings);
            }

            // 5. 批量删除用户（直接使用 dto 中的 userId）
            List<String> deleteUserIds = deleteDtos.stream()
                    .map(UserExcelImportDto::getUserId)
                    .filter(id -> id != null)
                    .toList();

            if (!deleteUserIds.isEmpty()) {
                // 1. 批量删除用户角色映射
                roleMappingService.remove(
                        new LambdaQueryWrapper<RoleMapping>()
                                .in(RoleMapping::getUserId, deleteUserIds));
                // 2. 批量删除用户组映射
                userGroupMappingService.remove(
                        new LambdaQueryWrapper<cn.opensrcdevelop.auth.biz.entity.user.group.UserGroupMapping>()
                                .in(cn.opensrcdevelop.auth.biz.entity.user.group.UserGroupMapping::getUserId,
                                        deleteUserIds));
                // 3. 批量删除扩展属性
                userAttrMappingService.remove(
                        new LambdaQueryWrapper<UserAttrMapping>()
                                .in(UserAttrMapping::getUserId, deleteUserIds));
                // 4. 批量删除用户
                userService.removeBatchByIds(deleteUserIds);
                deletedCount = deleteUserIds.size();
            }

        } catch (Exception e) {
            log.error("批量数据库操作失败", e);
            for (UserExcelImportDto dto : validData) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(validData.indexOf(dto) + 2)
                        .column("数据处理")
                        .message("批量操作失败: " + e.getMessage())
                        .build());
            }
            return new ImportStats(0, 0, 0);
        }

        return new ImportStats(createDtos.size(), updateDtos.size(), deletedCount);
    }

    /**
     * 从 Excel 构建用户实体（基础字段）
     */
    private User buildUserFromExcel(UserExcelImportDto dto, List<UserAttrResponseDto> extAttrs, String userId) {
        User user = new User();
        if (userId != null) {
            user.setUserId(userId);
        } else if (dto.getOperationType() == 0) {
            // 创建新用户时生成 userId
            user.setUserId(CommonUtil.getUUIDV7String());
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
            Set<String> existUsernames, Set<String> existEmails, Set<String> existPhones,
            Map<String, String> keyToHeaderMap) {
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
                        .column(keyToHeaderMap.getOrDefault("username", "用户名*"))
                        .message("用户名已存在: " + dto.getUsername())
                        .build());
            }

            // 检查邮箱
            if (StringUtils.isNotBlank(dto.getEmailAddress()) && existEmails.contains(dto.getEmailAddress())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("emailAddress", "邮箱地址*"))
                        .message("邮箱已被注册: " + dto.getEmailAddress())
                        .build());
            }

            // 检查手机号
            if (StringUtils.isNotBlank(dto.getPhoneNumber()) && existPhones.contains(dto.getPhoneNumber())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("phoneNumber", "手机号码"))
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
            List<ExcelImportErrorDto> errors, Map<String, String> keyToHeaderMap) {
        // 收集用户名
        if (StringUtils.isNotBlank(dto.getUsername())) {
            if (!excelUsernames.add(dto.getUsername())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("username", "用户名*"))
                        .message("用户名在 Excel 中重复")
                        .build());
            }
        }

        // 收集邮箱
        if (StringUtils.isNotBlank(dto.getEmailAddress())) {
            if (!excelEmails.add(dto.getEmailAddress())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("emailAddress", "邮箱地址*"))
                        .message("邮箱在 Excel 中重复")
                        .build());
            }
        }

        // 收集手机号
        if (StringUtils.isNotBlank(dto.getPhoneNumber())) {
            if (!excelPhones.add(dto.getPhoneNumber())) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("phoneNumber", "手机号码"))
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
            Set<String> excelUsernames, Set<String> excelEmails, Set<String> excelPhones,
            Map<String, String> keyToHeaderMap) {
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
                    if (StringUtils.isBlank(dto.getUserId())) {
                        rowErrors.add("用户ID不能为空");
                    }
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
            // 跳过空值和 null 值
            if (attr == null || value == null || "null".equals(String.valueOf(value))) {
                continue;
            }
            String valueStr = String.valueOf(value);
            if (StringUtils.isBlank(valueStr)) {
                continue;
            }
            String dataType = attr.getDataType();

            // 根据数据类型验证格式
            switch (dataType) {
                case "NUMBER" :
                    try {
                        Double.parseDouble(valueStr);
                    } catch (NumberFormatException e) {
                        rowErrors.add("必须是数字");
                    }
                    break;
                case "DATE" :
                    // 注意：日期在 UserExcelImportHandler 中已转换为时间戳（12-13位数字）
                    // 如果值是12-13位数字，说明转换成功；否则说明原值格式有问题
                    int len = valueStr.length();
                    boolean isValidLength = (len == 12 || len == 13);
                    if (!isValidLength || !isValidTimestamp(valueStr)) {
                        rowErrors.add("日期格式无效，正确格式：20260109");
                    }
                    break;
                case "DATETIME" :
                    // 注意：日期时间在 UserExcelImportHandler 中已转换为时间戳（12-13位数字）
                    // 如果值是12-13位数字，说明转换成功；否则说明原值格式有问题
                    len = valueStr.length();
                    isValidLength = (len == 12 || len == 13);
                    if (!isValidLength || !isValidTimestamp(valueStr)) {
                        rowErrors.add("日期时间格式无效，正确格式：20260109163045");
                    }
                    break;
                case "BOOLEAN" :
                    // 验证布尔值：是/否
                    if (!"是".equals(valueStr) && !"否".equals(valueStr)) {
                        rowErrors.add("必须是 是 或 否");
                    }
                    break;
                case "DICT" :
                    // 验证字典值是否在有效范围内
                    Map<String, String> valueToIdMap = dictValueToIdMap.get(key);
                    if (valueToIdMap != null && !valueToIdMap.isEmpty()) {
                        // 检查值是否在 key（显示文本）中
                        boolean isValidKey = valueToIdMap.containsKey(valueStr);
                        // 检查值是否在 value（UUID）中
                        boolean isValidValue = valueToIdMap.containsValue(valueStr);

                        if (!isValidKey && !isValidValue) {
                            rowErrors.add("值不在有效范围内");
                        } else if (isValidKey) {
                            // 值是显示文本，找到对应的 UUID 并存储
                            String uuid = valueToIdMap.get(valueStr);
                            dto.getExtAttrs().put(key, uuid);
                        }
                        // 如果 isValidValue 为 true，保持原值不变（已经是 UUID）
                    }
                    break;
                default :
                    break;
            }
        }

        if (rowErrors.isEmpty()) {
            validData.add(dto);
        } else {
            for (String error : rowErrors) {
                // 从错误消息中提取字段名（格式：字段 [xxx] 的...）
                String column = extractFieldNameFromError(error, keyToHeaderMap);
                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(column)
                        .message(error)
                        .build());
            }
        }
    }

    /**
     * 从错误消息中提取字段名 错误消息格式：字段 [xxx] 的值不在有效范围内 如果无法提取，返回实际字段标题
     */
    private String extractFieldNameFromError(String error, Map<String, String> keyToHeaderMap) {
        if (error == null) {
            return "未知";
        }

        // 尝试从错误消息中提取字段名（格式：字段 [xxx] 的...）
        int start = error.indexOf('[');
        int end = error.indexOf(']');
        if (start >= 0 && end > start) {
            String fieldName = error.substring(start + 1, end);
            // 如果提取到的字段名已经在 keyToHeaderMap 中有映射，返回映射后的中文标题
            // 否则直接返回提取到的字段名
            return keyToHeaderMap.getOrDefault(fieldName, fieldName);
        }

        // 对于没有 "字段 [xxx]" 格式的错误消息，只有操作类型可以硬编码处理
        if (error.contains("操作类型")) {
            return "操作类型";
        }

        // 如果无法从错误消息提取，返回"未知"
        return "未知";
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
     * 验证时间戳格式（12-13位数字，毫秒级时间戳）
     */
    private boolean isValidTimestamp(String value) {
        if (value == null) {
            return false;
        }
        int len = value.length();
        if (len != 12 && len != 13) {
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
