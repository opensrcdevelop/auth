package cn.opensrcdevelop.auth.biz.service.user.excel.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.DataFilterEnum;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.dict.DictDataResponseDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportErrorDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.ExcelImportResultDto;
import cn.opensrcdevelop.auth.biz.dto.user.excel.UserExcelImportDto;
import cn.opensrcdevelop.auth.biz.entity.role.RoleMapping;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroupMapping;
import cn.opensrcdevelop.auth.biz.service.role.RoleMappingService;
import cn.opensrcdevelop.auth.biz.service.system.mail.MailService;
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
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import com.alibaba.excel.EasyExcelFactory;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.redisson.api.RLock;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

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
    private final MailService mailService;

    private static final String EMAIL_ADDRESS_REGEX = "^[a-zA-Z0-9_+&*-]+(?:\\.[a-zA-Z0-9_+&*-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,7}$";
    private static final String PHONE_NUMBER_REGEX = "^1[3-9]\\d{9}$";
    private static final String IMPORT_USER_LOCK = "import_user_lock";

    @Audit(type = AuditType.SYS_OPERATION, sysOperation = SysOperationType.DOWNLOAD, resource = ResourceType.USER, success = "下载了用户导入模版", fail = "下载用户导入模版失败")
    @Override
    public byte[] generateImportTemplate() {
        // 获取所有用户字段（包括基础字段和扩展字段）
        List<UserAttrResponseDto> allFields = userAttrService.getAllUserAttrsForExcel();
        // 导入模版不需要 createTime 字段（创建时自动生成）
        List<UserAttrResponseDto> templateFields = allFields.stream()
                .filter(f -> !"createTime".equals(f.getKey()))
                .toList();
        return templateGenerator.generateTemplate(templateFields);
    }

    @Audit(type = AuditType.SYS_OPERATION, sysOperation = SysOperationType.EXPORT, resource = ResourceType.USER, success = "导出了共 {{ #total }} 条用户数据（{{ #exportAll ? '导出全部' : '导出当前页' }}），筛选条件（{{ #hasFilters }}）", fail = "导出用户数据失败（{{ #exportAll ? '导出全部' : '导出当前页' }}）")
    @Override
    public byte[] exportUsers(List<DataFilterDto> filters, boolean exportAll, List<String> userIds) {
        // 1. 获取所有用户字段（包括基础字段和扩展字段）
        List<UserAttrResponseDto> allFields = userAttrService.getAllUserAttrsForExcel();

        // 2. 获取用户数据
        List<Map<String, Object>> userList;
        if (userIds != null && !userIds.isEmpty()) {
            // 导出指定用户ID列表（当前页）
            DataFilterDto filter = new DataFilterDto();
            filter.setDataType(UserAttrDataTypeEnum.STRING.getType());
            filter.setKey(CommonUtil.extractFieldNameFromGetter(User::getUserId));
            filter.setExtFlg(false);
            filter.setFilterType(DataFilterEnum.IN.getType());
            filter.setValue(StringUtils.join(userIds, CommonConstants.COMMA));
            userList = userService.list(1, Integer.MAX_VALUE, List.of(filter)).getList();
        } else {
            // 导出全部或按条件筛选
            List<DataFilterDto> safeFilters = filters != null ? filters : new ArrayList<>();
            PageData<Map<String, Object>> users = userService.list(1, Integer.MAX_VALUE, safeFilters);
            userList = users.getList() != null ? users.getList() : new ArrayList<>();
        }

        AuditContext.setSpelVariable("total", userList.size());
        AuditContext.setSpelVariable("hasFilters", Objects.isNull(filters) ? "无" : filters.size() + " 条");

        // 3. 使用新的导出器生成 Excel
        return userExcelExporter.exportUsers(userList, allFields, filters);
    }

    @Audit(type = AuditType.SYS_OPERATION, sysOperation = SysOperationType.IMPORT, resource = ResourceType.USER, success = "{{ #excelImportResult.errors.size() == 0 ? '导入用户数据成功（ ' + #excelImportResult.createdCount + ' 条新增，' + #excelImportResult.updatedCount + ' 条更新，' + #excelImportResult.deletedCount + ' 条删除）' : '数据校验失败，无法导入用户数据（ ' + #excelImportResult.errors.size() + ' 条错误）'}}", fail = "导入用户数据失败")
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ExcelImportResultDto importUsers(MultipartFile file) {
        if (RedisUtil.getLock(IMPORT_USER_LOCK).isLocked()) {
            throw new BizException(MessageConstants.USER_MSG_1003);
        }

        RLock lock = RedisUtil.getLock(IMPORT_USER_LOCK);
        try {
            if (!lock.tryLock()) {
                throw new BizException(MessageConstants.USER_MSG_1003);
            }
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
                    if (UserAttrDataTypeEnum.DICT.getType().equals(attr.getDataType()) && attr.getDictId() != null) {
                        dictValueToIdMap.put(attr.getKey(), loadDictValueToIdMap(attr.getDictId()));
                    }
                }
            }

            // 2. 读取 Excel 数据并收集所有用户名、邮箱、手机号
            Set<String> excelUsernames = new HashSet<>();
            Set<String> excelEmails = new HashSet<>();
            Set<String> excelPhones = new HashSet<>();
            // 字段 key -> 中文标题的映射（用于错误显示）
            Map<String, String> keyToHeaderMap;

            // 读取文件内容到字节数组（避免多次调用 getInputStream）
            byte[] fileBytes;
            try {
                fileBytes = file.getBytes();
            } catch (IOException e) {
                throw new ServerException("读取上传文件失败", e);
            }
            if (fileBytes.length == 0) {
                ExcelImportResultDto excelImportResult = ExcelImportResultDto.builder()
                        .createdCount(0)
                        .updatedCount(0)
                        .deletedCount(0)
                        .errors(errors)
                        .build();
                AuditContext.setSpelVariable("excelImportResult", excelImportResult);
                return excelImportResult;
            }

            // 使用字节数组创建 InputStream
            try (ByteArrayInputStream excelStream = new ByteArrayInputStream(fileBytes)) {
                // 使用自定义处理器读取 Excel（支持中文标题映射）
                UserExcelImportHandler handler = new UserExcelImportHandler(attrMap, dictValueToIdMap, excelStream);
                // 获取字段 key -> 中文标题的映射（用于错误显示）
                keyToHeaderMap = handler.getKeyToHeaderMap();

                // 重置流位置（因为 UserExcelImportHandler 读取后位置已移动）
                excelStream.reset();
                EasyExcelFactory.read(excelStream)
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
                    validateBasicRules(dto, rowNum, errors, validData, attrMap, dictValueToIdMap, keyToHeaderMap);
                }
            } catch (Exception e) {
                log.error("读取 Excel 文件失败", e);
                throw new ServerException("读取 Excel 文件失败", e);
            }

            // 3. 批量查询数据库中已存在的用户名、邮箱、手机号
            Set<String> existUsernames = batchQueryExistingUsers(excelUsernames,
                    CommonUtil.extractFieldNameFromGetter(User::getUsername));
            Set<String> existEmails = batchQueryExistingUsers(excelEmails,
                    CommonUtil.extractFieldNameFromGetter(User::getEmailAddress));
            Set<String> existPhones = batchQueryExistingUsers(excelPhones,
                    CommonUtil.extractFieldNameFromGetter(User::getPhoneNumber));

            // 4. 检查新增/更新用户是否与数据库重复
            List<ExcelImportErrorDto> dbDuplicateErrors = checkDbDuplicates(validData, existUsernames, existEmails,
                    existPhones, keyToHeaderMap);
            errors.addAll(dbDuplicateErrors);

            // 5. 如果有错误，不执行数据库操作
            if (!errors.isEmpty()) {
                ExcelImportResultDto excelImportResult = ExcelImportResultDto.builder()
                        .createdCount(0)
                        .updatedCount(0)
                        .deletedCount(0)
                        .errors(errors)
                        .build();
                AuditContext.setSpelVariable("excelImportResult", excelImportResult);
                return excelImportResult;
            }

            // 6. 批量执行数据库操作（只有所有数据校验通过才会执行到这里）
            ImportStats stats = batchExecuteDatabaseOperations(validData, extAttrs, errors);

            ExcelImportResultDto excelImportResult = ExcelImportResultDto.builder()
                    .createdCount(stats.createdCount)
                    .updatedCount(stats.updatedCount)
                    .deletedCount(stats.deletedCount)
                    .errors(errors)
                    .build();
            AuditContext.setSpelVariable("excelImportResult", excelImportResult);
            return excelImportResult;
        } finally {
            if (lock.isLocked()) {
                lock.unlock();
            }
        }
    }

    /**
     * 导入统计结果
     */
    private record ImportStats(int createdCount, int updatedCount, int deletedCount) {
    }

    /**
     * 批量执行数据库操作
     */
    @SuppressWarnings("java:S3776")
    private ImportStats batchExecuteDatabaseOperations(List<UserExcelImportDto> validData,
            List<UserAttrResponseDto> extAttrs,
            List<ExcelImportErrorDto> errors) {
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

        // 2. 批量创建用户
        List<Tuple2<User, String>> createUsers = new ArrayList<>();
        List<UserAttrMapping> createAttrMappings = new ArrayList<>();
        for (UserExcelImportDto dto : createDtos) {
            Tuple2<User, String> userRes = buildUserFromExcel(dto);
            createUsers.add(userRes);
            dto.setUserId(userRes._1().getUserId());
        }

        try {
            // 批量保存用户
            if (!createUsers.isEmpty()) {
                userService.saveBatch(CommonUtil.stream(createUsers).map(Tuple2::_1).toList());
            }

            if (!createDtos.isEmpty()) {
                // 构建新用户的扩展属性映射
                for (UserExcelImportDto dto : createDtos) {
                    List<UserAttrMapping> mappings = buildAttrMappingsFromExcel(dto, extAttrs);
                    createAttrMappings.addAll(mappings);
                }

                // 批量保存扩展属性
                if (!createAttrMappings.isEmpty()) {
                    userAttrMappingService.saveBatch(createAttrMappings);
                }
            }

            // 3. 批量更新用户
            List<User> updateUsers = new ArrayList<>();
            // 收集更新用户的扩展属性映射
            List<UserAttrMapping> updateAttrMappings = new ArrayList<>();

            // 批量查询所有需要更新用户的现有扩展属性（直接使用 dto 中的 userId）
            Set<String> allUserIdsForUpdate = updateDtos.stream()
                    .map(UserExcelImportDto::getUserId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());

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

                User user = buildUserFromExcel(dto)._1;
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

            // 4. 批量删除用户（直接使用 dto 中的 userId）
            List<String> deleteUserIds = deleteDtos.stream()
                    .map(UserExcelImportDto::getUserId)
                    .filter(Objects::nonNull)
                    .toList();

            if (!deleteUserIds.isEmpty()) {
                // 4.1. 批量删除用户角色映射
                roleMappingService.remove(
                        new LambdaQueryWrapper<RoleMapping>()
                                .in(RoleMapping::getUserId, deleteUserIds));
                // 4.2. 批量删除用户组映射
                userGroupMappingService.remove(
                        new LambdaQueryWrapper<UserGroupMapping>()
                                .in(UserGroupMapping::getUserId,
                                        deleteUserIds));
                // 4.3. 批量删除扩展属性
                userAttrMappingService.remove(
                        new LambdaQueryWrapper<UserAttrMapping>()
                                .in(UserAttrMapping::getUserId, deleteUserIds));
                // 4.4. 批量删除用户
                userService.removeBatchByIds(deleteUserIds);
                deletedCount = deleteUserIds.size();
            }

            // 5. 为新创建的用户发送邮件
            if (!createUsers.isEmpty()) {
                CompletableFuture.runAsync(() -> CommonUtil.stream(createUsers)
                        .filter(userRes -> StringUtils.isNotBlank(userRes._1.getEmailAddress()))
                        .forEach(userRes -> mailService.sendCreateUserNotice(userRes._1.getEmailAddress(),
                                userRes._1.getUsername(), userRes._2)));
            }
        } catch (Exception e) {
            log.error("批量数据库操作失败", e);
            for (UserExcelImportDto dto : validData) {
                errors.add(ExcelImportErrorDto.builder()
                        .row(validData.indexOf(dto) + 2)
                        .message("数据库操作失败")
                        .build());
            }
            return new ImportStats(0, 0, 0);
        }

        return new ImportStats(createDtos.size(), updateDtos.size(), deletedCount);
    }

    /**
     * 从 Excel 构建用户实体（基础字段）
     */
    private Tuple2<User, String> buildUserFromExcel(UserExcelImportDto dto) {
        User user = new User();
        String newPwd = null;

        user.setUserId(dto.getUserId());
        if (dto.getOperationType() == 0) {
            // 创建新用户时生成 userId
            user.setUserId(CommonUtil.getUUIDV7String());
        }
        user.setUsername(dto.getUsername());
        // 只有创建用户时才生成随机密码并加密
        if (dto.getOperationType() == 0) {
            newPwd = CommonUtil.generateRandomString(12);
            user.setPassword(passwordEncoder.encode(newPwd));
            user.setNeedChangePwd(true);
        }
        user.setEmailAddress(dto.getEmailAddress());
        user.setPhoneNumber(dto.getPhoneNumber());
        user.setConsoleAccess(dto.getConsoleAccess());
        user.setEnableMfa(dto.getEnableMfa());
        user.setLocked(dto.getLocked());

        return Tuple.of(user, newPwd);
    }

    /**
     * 从 Excel 构建扩展属性映射列表
     */
    private List<UserAttrMapping> buildAttrMappingsFromExcel(UserExcelImportDto dto,
            List<UserAttrResponseDto> extAttrs) {
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

            UserAttrMapping mapping = new UserAttrMapping();
            mapping.setUserId(dto.getUserId());
            mapping.setAttrId(attr.getId());
            mapping.setAttrValue(value.toString());
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
                .in(fieldName.equals(CommonUtil.extractFieldNameFromGetter(User::getUsername)), User::getUsername,
                        values)
                .in(fieldName.equals(CommonUtil.extractFieldNameFromGetter(User::getEmailAddress)),
                        User::getEmailAddress, values)
                .in(fieldName.equals(CommonUtil.extractFieldNameFromGetter(User::getPhoneNumber)), User::getPhoneNumber,
                        values)
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
    @SuppressWarnings("java:S3776")
    private List<ExcelImportErrorDto> checkDbDuplicates(List<UserExcelImportDto> validData,
            Set<String> existUsernames, Set<String> existEmails, Set<String> existPhones,
            Map<String, String> keyToHeaderMap) {
        List<ExcelImportErrorDto> errors = new ArrayList<>();

        // 批量获取更新用户的用户名、邮箱、手机号
        Set<String> updateUserIds = CommonUtil.stream(validData)
                .filter(dto -> dto.getOperationType() == 1)
                .map(UserExcelImportDto::getUserId).collect(Collectors.toSet());
        Map<String, User> updateUsers = new HashMap<>();
        if (CollectionUtils.isNotEmpty(updateUserIds)) {
            updateUsers = userService.lambdaQuery()
                    .in(User::getUserId, updateUserIds)
                    .list()
                    .stream()
                    .collect(Collectors.toMap(User::getUserId, Function.identity()));
        }

        for (int i = 0; i < validData.size(); i++) {
            UserExcelImportDto dto = validData.get(i);
            int rowNum = i + 3;

            if (dto.getOperationType() == null || dto.getOperationType() == 2) {
                continue; // 只检查新增/更新操作
            }

            // 检查用户名
            if (StringUtils.isNotBlank(dto.getUsername()) && existUsernames.contains(dto.getUsername())) {
                // 检查更新操作时，用户名与当前用户一致，忽略错误
                if (dto.getOperationType() == 1
                        && updateUsers.get(dto.getUserId()).getUsername().equals(dto.getUsername())) {
                    continue;
                }

                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("username", "用户名"))
                        .message("用户名已存在: " + dto.getUsername())
                        .build());
            }

            // 检查邮箱
            if (StringUtils.isNotBlank(dto.getEmailAddress()) && existEmails.contains(dto.getEmailAddress())) {
                // 检查更新操作时，邮箱与当前用户一致，忽略错误
                if (dto.getOperationType() == 1
                        && updateUsers.get(dto.getUserId()).getEmailAddress().equals(dto.getEmailAddress())) {
                    continue;
                }

                errors.add(ExcelImportErrorDto.builder()
                        .row(rowNum)
                        .column(keyToHeaderMap.getOrDefault("emailAddress", "邮箱地址"))
                        .message("邮箱已被注册: " + dto.getEmailAddress())
                        .build());
            }

            // 检查手机号
            if (StringUtils.isNotBlank(dto.getPhoneNumber()) && existPhones.contains(dto.getPhoneNumber())) {
                // 检查更新操作时，手机号与当前用户一致，忽略错误
                if (dto.getOperationType() == 1
                        && updateUsers.get(dto.getUserId()).getPhoneNumber().equals(dto.getPhoneNumber())) {
                    continue;
                }

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
        if (StringUtils.isNotBlank(dto.getUsername()) && !excelUsernames.add(dto.getUsername())) {
            errors.add(ExcelImportErrorDto.builder()
                    .row(rowNum)
                    .column(keyToHeaderMap.getOrDefault("username", "用户名"))
                    .message("用户名在 Excel 中重复")
                    .build());
        }

        // 收集邮箱
        if (StringUtils.isNotBlank(dto.getEmailAddress()) && !excelEmails.add(dto.getEmailAddress())) {
            errors.add(ExcelImportErrorDto.builder()
                    .row(rowNum)
                    .column(keyToHeaderMap.getOrDefault("emailAddress", "邮箱地址"))
                    .message("邮箱在 Excel 中重复")
                    .build());
        }

        // 收集手机号
        if (StringUtils.isNotBlank(dto.getPhoneNumber()) && !excelPhones.add(dto.getPhoneNumber())) {
            errors.add(ExcelImportErrorDto.builder()
                    .row(rowNum)
                    .column(keyToHeaderMap.getOrDefault("phoneNumber", "手机号码"))
                    .message("手机号在 Excel 中重复")
                    .build());
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
     * 验证基础规则（格式验证、必填验证等）
     */
    @SuppressWarnings("java:S3776")
    private void validateBasicRules(UserExcelImportDto dto, int rowNum, List<ExcelImportErrorDto> errors,
            List<UserExcelImportDto> validData, Map<String, UserAttrResponseDto> attrMap,
            Map<String, Map<String, String>> dictValueToIdMap, Map<String, String> keyToHeaderMap) {
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
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault("username", "用户名"))
                                        .message("用户名不能为空")
                                        .build());
                    }
                    if (StringUtils.isBlank(dto.getEmailAddress())) {
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault("emailAddress", "邮箱地址"))
                                        .message("邮箱地址不能为空")
                                        .build());
                    }
                    break;
                case 1, 2 : // 删除
                    if (StringUtils.isBlank(dto.getUserId())) {
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault("userId", "用户ID"))
                                        .message("用户ID不能为空")
                                        .build());
                    }
                    break;
                default :
                    break;
            }
        }

        // 验证邮箱地址格式
        if (StringUtils.isNotEmpty(dto.getEmailAddress()) && !isValidEmailAddress(dto.getEmailAddress())) {
            errors.add(
                    ExcelImportErrorDto.builder()
                            .row(rowNum)
                            .column(keyToHeaderMap.getOrDefault("emailAddress", "邮箱地址"))
                            .message("邮箱地址格式无效")
                            .build());
        }

        // 验证手机号格式
        if (StringUtils.isNotEmpty(dto.getPhoneNumber()) && !isValidPhoneNumber(dto.getPhoneNumber())) {
            errors.add(
                    ExcelImportErrorDto.builder()
                            .row(rowNum)
                            .column(keyToHeaderMap.getOrDefault("phoneNumber", "手机号"))
                            .message("手机号格式无效")
                            .build());
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
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault(key, key))
                                        .message("必须是数字")
                                        .build());
                    }
                    break;
                case "DATE" :
                    // 注意：日期在 UserExcelImportHandler 中已转换为时间戳（12-13位数字）
                    // 如果值是12-13位数字，说明转换成功；否则说明原值格式有问题
                    if (isValidTimestamp(valueStr)) {
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault(key, key))
                                        .message("日期格式无效，正确格式：yyyymmdd")
                                        .build());
                    }
                    break;
                case "DATETIME" :
                    // 注意：日期时间在 UserExcelImportHandler 中已转换为时间戳（12-13位数字）
                    // 如果值是12-13位数字，说明转换成功；否则说明原值格式有问题
                    if (isValidTimestamp(valueStr)) {
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault(key, key))
                                        .message("日期时间格式无效，正确格式：yyyymmddhhmmss")
                                        .build());
                    }
                    break;
                case "BOOLEAN" :
                    // 验证布尔值：是/否
                    if (!"是".equals(valueStr) && !"否".equals(valueStr)) {
                        errors.add(
                                ExcelImportErrorDto.builder()
                                        .row(rowNum)
                                        .column(keyToHeaderMap.getOrDefault(key, key))
                                        .message("必须是 是 或 否")
                                        .build());
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
                            errors.add(
                                    ExcelImportErrorDto.builder()
                                            .row(rowNum)
                                            .column(keyToHeaderMap.getOrDefault(key, key))
                                            .message("值不在有效范围内")
                                            .build());
                        } else if (isValidKey) {
                            // 值是显示文本，找到对应的 UUID 并存储
                            String uuid = valueToIdMap.get(valueStr);
                            dto.getExtAttrs().put(key, uuid);
                        }
                    }
                    break;
                default :
                    break;
            }
        }

        if (rowErrors.isEmpty()) {
            validData.add(dto);
        }
    }

    /**
     * 验证时间戳格式（12-13位数字，毫秒级时间戳）
     */
    private boolean isValidTimestamp(String value) {
        if (value == null) {
            return true;
        }

        int len = value.length();
        if (len != 12 && len != 13) {
            return true;
        }

        try {
            Long.parseLong(value);
            return false;
        } catch (NumberFormatException e) {
            return true;
        }
    }

    private boolean isValidEmailAddress(String emailAddress) {
        if (StringUtils.isNotBlank(emailAddress)) {
            return emailAddress.matches(EMAIL_ADDRESS_REGEX);
        }
        return false;
    }

    private boolean isValidPhoneNumber(String phoneNumber) {
        if (StringUtils.isNotBlank(phoneNumber)) {
            return phoneNumber.matches(PHONE_NUMBER_REGEX);
        }
        return false;
    }
}
