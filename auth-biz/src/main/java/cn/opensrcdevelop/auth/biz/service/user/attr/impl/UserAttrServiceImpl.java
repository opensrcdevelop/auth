package cn.opensrcdevelop.auth.biz.service.user.attr.impl;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.constants.UserAttrDataTypeEnum;
import cn.opensrcdevelop.auth.biz.dto.user.attr.SetUserAttrDisplaySeqRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrRequestDto;
import cn.opensrcdevelop.auth.biz.dto.user.attr.UserAttrResponseDto;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttrMapping;
import cn.opensrcdevelop.auth.biz.entity.user.attr.dict.DictData;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.UserAttrMapper;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.UserAttrMappingMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.UserAttrRepository;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrMappingService;
import cn.opensrcdevelop.auth.biz.service.user.attr.UserAttrService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictDataService;
import cn.opensrcdevelop.auth.biz.service.user.attr.dict.DictService;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.batch.MybatisBatch;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class UserAttrServiceImpl extends ServiceImpl<UserAttrMapper, UserAttr> implements UserAttrService {

    private final UserAttrMappingService userAttrMappingService;
    private final UserAttrRepository userAttrRepository;
    private final DictDataService dictDataService;

    @Resource
    @Lazy
    private DictService dictService;

    /**
     * 创建用户属性
     *
     * @param requestDto 创建用户属性请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_ATTR,
            sysOperation = SysOperationType.CREATE,
            success = "创建了用户属性（{{ @linkGen.toLink(#userAttrId, T(ResourceType).USER_ATTR) }}）",
            fail = "创建用户属性（{{ #requestDto.name }}）失败"
    )
    @Transactional
    @Override
    public void createUserAttr(UserAttrRequestDto requestDto) {
        // 1. 检查属性 Key 是否存在
        if (Objects.nonNull(super.getOne(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getAttrKey, requestDto.getKey())))) {
            throw new BizException(MessageConstants.USER_ATTR_MSG_1000, requestDto.getKey());
        }

        // 2. 属性编辑
        String userAttrId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("userAttrId", userAttrId);
        UserAttr userAttr = new UserAttr();
        userAttr.setAttrId(userAttrId);
        userAttr.setAttrKey(requestDto.getKey());
        userAttr.setAttrName(requestDto.getName());
        userAttr.setAttrDataType(requestDto.getDataType());
        userAttr.setExtAttrFlg(requestDto.getExtFlg());
        userAttr.setUserVisible(requestDto.getUserVisible());
        userAttr.setUserEditable(requestDto.getUserEditable());
        Boolean display = requestDto.getUserLstDisplay();
        userAttr.setUserLstDisplay(display);
        CommonUtil.callSetWithCheck(Objects::nonNull, userAttr::setDisplayWidth, requestDto::getDisplayWidth);

        // 2.1 若在用户列表显示，则获取最大显示顺序
        if (Boolean.TRUE.equals(display)) {
            userAttr.setDisplaySeq(userAttrRepository.getMaxDisplaySeq());
        }

        // 2.2 属性数据类型为 DICT，设置字典ID
        if (UserAttrDataTypeEnum.DICT.getType().equals(requestDto.getDataType()) && StringUtils.isNotEmpty(requestDto.getDictId())) {
            userAttr.setDictId(requestDto.getDictId());
        }

        // 3. 数据库操作
        super.save(userAttr);
    }


    /**
     * 设置用户的扩展属性
     *
     * @param userId     用户 ID
     * @param attributes 用户的扩展属性
     */
    @Transactional
    @Override
    public void createUserAttrMapping(String userId, List<UserAttrMappingRequestDto> attributes) {
        // 1. 检查用户属性数据类型为 DICT 的请求是否有效
        checkAttrValueForDict(attributes);

        // 2. 属性编辑
        var mappings = CommonUtil.stream(attributes).map(attribute -> {
            UserAttrMapping mapping = new UserAttrMapping();
            mapping.setUserId(userId);
            mapping.setAttrId(attribute.getAttrId());
            mapping.setAttrValue(attribute.getAttrValue());
            return mapping;
        }).toList();

        // 3. 数据库操作
        userAttrMappingService.saveBatch(mappings);
    }

    /**
     * 获取用户的属性
     *
     * @param userId 用户 ID
     * @return 用户的属性
     */
    @Override
    public List<UserAttr> getUserAttrs(String userId) {
        // 1. 获取全部扩展用户属性
        List<UserAttr> allUserAttrs = super.list(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getExtAttrFlg, true));

        // 2. 获取用户的扩展属性
        List<UserAttr> userAttrs = userAttrRepository.searchUserAttrs(userId);

        // 3. 合并值
        for (UserAttr userAttr : allUserAttrs) {
            CommonUtil.stream(userAttrs)
                    .filter(x -> x.getAttrId().equals(userAttr.getAttrId()))
                    .findFirst()
                    .ifPresent(x -> userAttr.setAttrValue(x.getAttrValue()));
        }
        return allUserAttrs;
    }

    /**
     * 获取所有用户属性
     *
     * @param page      页数
     * @param size      条数
     * @param onDisplay 返回只在用户列表显示的用户属性
     * @param keyword   用户属性名称或 key 检索关键字
     * @return 所有用户属性
     */
    @Override
    public PageData<UserAttrResponseDto> listUserAttrs(int page, int size, Boolean onDisplay, String keyword) {
        List<UserAttr> userAttrs;
        Page<UserAttr> pageRequest = new Page<>(page, size);
        // 1. 数据库操作
        var query = Wrappers.<UserAttr>lambdaQuery();
        if (StringUtils.isNotEmpty(keyword)) {
            query = query.like(UserAttr::getAttrKey, keyword).or(o -> o.like(UserAttr::getAttrName, keyword));
        }
        if (Boolean.TRUE.equals(onDisplay)) {
            userAttrs = super.list(pageRequest, query.and(o -> o.eq(UserAttr::getUserLstDisplay, true)).orderByAsc(UserAttr::getDisplaySeq));
        } else {
            userAttrs = super.list(pageRequest, query.orderByAsc(UserAttr::getAttrKey));
        }

        // 2. 属性设置
        PageData<UserAttrResponseDto> pageData = new PageData<>();
        pageData.setTotal(pageRequest.getTotal());
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setSize(pageRequest.getSize());

        var records = CommonUtil.stream(userAttrs).map(userAttr -> {
            UserAttrResponseDto userAttrResponse = new UserAttrResponseDto();
            userAttrResponse.setId(userAttr.getAttrId());
            userAttrResponse.setKey(userAttr.getAttrKey());
            userAttrResponse.setName(userAttr.getAttrName());
            userAttrResponse.setDataType(userAttr.getAttrDataType());
            userAttrResponse.setExtFlg(userAttr.getExtAttrFlg());
            userAttrResponse.setUserLstDisplay(userAttr.getUserLstDisplay());
            userAttrResponse.setDisplaySeq(userAttr.getDisplaySeq());
            userAttrResponse.setDisplayWidth(userAttr.getDisplayWidth());
            userAttrResponse.setDictId(userAttr.getDictId());

            // 2.1 属性数据类型为 DICT，设置是否是级联字典
            if (userAttr.getDictId() != null) {
                userAttrResponse.setCascadeDict(dictService.hasChildDict(userAttr.getDictId()));
            }

            return userAttrResponse;
        }).toList();
        pageData.setList(records);
        return pageData;
    }

    /**
     * 更新用户的扩展属性
     *
     * @param userId     用户 ID
     * @param attributes 用户的扩展属性
     */
    @Transactional
    @Override
    public void updateUserUserAttrMapping(String userId, List<UserAttrMappingRequestDto> attributes) {
        if (CollectionUtils.isEmpty(attributes)) {
            return;
        }

        // 1. 值为空：删除用户和用户属性的映射关系
        // 1.1 获取并移除待删除的映射关系
        List<UserAttrMappingRequestDto> deleteTargets = new ArrayList<>();
        for (var attribute : attributes) {
            if (StringUtils.isEmpty(attribute.getAttrValue())) {
                deleteTargets.add(attribute);
            }
        }
        attributes.removeAll(deleteTargets);

        // 1.2 数据库操作
        if (!deleteTargets.isEmpty()) {
            userAttrMappingService.remove(Wrappers.<UserAttrMapping>lambdaQuery()
                    .in(UserAttrMapping::getAttrId, deleteTargets.stream().map(UserAttrMappingRequestDto::getAttrId).toList())
                    .eq(UserAttrMapping::getUserId, userId));
        }

        // 2. 值非空：更新用户和用户属性的映射关系
        if (!attributes.isEmpty()) {
            // 2.1 检查用户属性数据类型为 DICT 的请求是否有效
            checkAttrValueForDict(attributes);
            if (!attributes.isEmpty()) {
                String attrIdColumn = com.baomidou.mybatisplus.core.toolkit.StringUtils.camelToUnderline(CommonUtil.extractFieldNameFromGetter(UserAttrMapping::getAttrId));
                String userIdColumn = com.baomidou.mybatisplus.core.toolkit.StringUtils.camelToUnderline(CommonUtil.extractFieldNameFromGetter(UserAttrMapping::getUserId));
                String attrValueColumn = com.baomidou.mybatisplus.core.toolkit.StringUtils.camelToUnderline(CommonUtil.extractFieldNameFromGetter(UserAttrMapping::getAttrValue));

                // 2.2 获取已存在的映射关系
                var existingMappings = userAttrMappingService.list(Wrappers.<UserAttrMapping>lambdaQuery()
                        .in(UserAttrMapping::getAttrId, attributes.stream().map(UserAttrMappingRequestDto::getAttrId).toList())
                        .eq(UserAttrMapping::getUserId, userId));

                // 2.3 向数据库中插入不存在的映射关系
                var notExistsAttributes = getNotExistAttributes(attributes, existingMappings);
                if (CollectionUtils.isNotEmpty(notExistsAttributes)) {
                    var mappings = notExistsAttributes.stream().map(a -> {
                        UserAttrMapping mapping = new UserAttrMapping();
                        mapping.setUserId(userId);
                        mapping.setAttrId(a.getAttrId());
                        mapping.setAttrValue(a.getAttrValue());
                        return mapping;
                    }).toList();
                    userAttrMappingService.saveBatch(mappings);
                }

                // 2.4 更新数据库中已存在的映射关系
                var notExistsAttrIds = notExistsAttributes.stream().map(UserAttrMappingRequestDto::getAttrId).toList();
                attributes.removeIf(a -> notExistsAttrIds.contains(a.getAttrId()));
                MybatisBatch<UserAttrMappingRequestDto> mybatisBatch = new MybatisBatch<>(getSqlSessionFactory(), attributes);
                MybatisBatch.Method<UserAttrMapping> method = new MybatisBatch.Method<>(UserAttrMappingMapper.class);
                mybatisBatch.execute(method.update(a -> Wrappers.<UserAttrMapping>update()
                                .set(attrValueColumn, a.getAttrValue())
                                .eq(userIdColumn, userId)
                                .and(o -> o.eq(attrIdColumn, a.getAttrId()))
                        )
                );
            }
        }
    }

    /**
     * 更新用户属性
     *
     * @param requestDto 更新用户属性请求
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_ATTR,
            sysOperation = SysOperationType.UPDATE,
            success = "修改了用户属性（{{ @linkGen.toLink(#userAttrId, T(ResourceType).USER_ATTR) }}）",
            fail = "修改用户属性（{{ @linkGen.toLink(#userAttrId, T(ResourceType).USER_ATTR) }}）失败"
    )
    @Transactional
    @Override
    public void updateUserAttr(UserAttrRequestDto requestDto) {
        String userAttrId = requestDto.getId();
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        // 1. 获取原用户属性
        var rawUserAttr = super.getById(userAttrId);
        if (Objects.isNull(rawUserAttr)) {
            return;
        }
        compareObjBuilder.id(userAttrId);
        compareObjBuilder.before(rawUserAttr);

        // 2. 属性设置
        UserAttr updateUserAttr = new UserAttr();
        updateUserAttr.setAttrId(userAttrId);
        updateUserAttr.setAttrName(requestDto.getName());
        CommonUtil.callSetWithCheck(Objects::nonNull, updateUserAttr::setUserVisible, requestDto::getUserVisible);
        CommonUtil.callSetWithCheck(Objects::nonNull, updateUserAttr::setUserEditable, requestDto::getUserEditable);
        CommonUtil.callSetWithCheck(Objects::nonNull, updateUserAttr::setUserLstDisplay, requestDto::getUserLstDisplay);
        CommonUtil.callSetWithCheck(Objects::nonNull, updateUserAttr::setDisplayWidth, requestDto::getDisplayWidth);

        // 2.1 设置显示顺序
        if (Boolean.TRUE.equals(requestDto.getUserLstDisplay())) {
            updateUserAttr.setDisplaySeq(userAttrRepository.getMaxDisplaySeq());
        }
        // 2.2 保留原显示顺序
        if (Objects.nonNull(requestDto.getDisplayWidth())) {
            updateUserAttr.setDisplaySeq(rawUserAttr.getDisplaySeq());
        }
        // 2.3 显示顺序为null，则不显示

        // 3. 数据库操作
        super.updateById(updateUserAttr);

        compareObjBuilder.after(super.getById(userAttrId));
        AuditContext.addCompareObj(compareObjBuilder.build());
    }

    /**
     * 设置用户属性显示顺序
     *
     * @param requestDtoList 设置用户属性显示顺序请求集合
     */
    @Transactional
    @Override
    public void setUserAttrDisplaySeq(List<SetUserAttrDisplaySeqRequestDto> requestDtoList) {
        // 1. 属性设置
        var updateList = CommonUtil.stream(requestDtoList).map(requestDto -> {
            UserAttr updateUserAttr = new UserAttr();
            updateUserAttr.setAttrId(requestDto.getId());
            updateUserAttr.setDisplaySeq(requestDto.getSeq());
            return updateUserAttr;
        }).toList();

        // 2. 数据库操作
        super.updateBatchById(updateList);
    }

    /**
     * 删除指定用户的全部属性信息
     *
     * @param userId 用户 ID
     */
    @Transactional
    @Override
    public void removeUserAttrMapping(String userId) {
        userAttrMappingService.remove(Wrappers.<UserAttrMapping>lambdaQuery().eq(UserAttrMapping::getUserId, userId));
    }

    /**
     * 获取用户属性详情
     *
     * @param userAttrId 用户属性ID
     * @return 用户属性详情
     */
    @Override
    public UserAttrResponseDto detail(String userAttrId) {
        // 1. 查询数据库
        UserAttr userAttr = super.getById(userAttrId);

        // 2. 属性设置
        UserAttrResponseDto userAttrResponse = new UserAttrResponseDto();
        userAttrResponse.setId(userAttr.getAttrId());
        userAttrResponse.setKey(userAttr.getAttrKey());
        userAttrResponse.setName(userAttr.getAttrName());
        userAttrResponse.setDataType(userAttr.getAttrDataType());
        userAttrResponse.setUserLstDisplay(userAttr.getUserLstDisplay());
        userAttrResponse.setExtFlg(userAttr.getExtAttrFlg());
        userAttrResponse.setDisplayWidth(userAttr.getDisplayWidth());
        userAttrResponse.setUserVisible(userAttr.getUserVisible());
        userAttrResponse.setUserEditable(userAttr.getUserEditable());
        userAttrResponse.setDictId(userAttr.getDictId());

        return userAttrResponse;
    }

    /**
     * 删除用户属性
     *
     * @param userAttrId 用户属性ID
     */
    @Audit(
            type = AuditType.SYS_OPERATION,
            resource = ResourceType.USER_ATTR,
            sysOperation = SysOperationType.DELETE,
            success = "删除了用户属性（{{ @linkGen.toLink(#userAttrId, T(ResourceType).USER_ATTR) }}）",
            fail = "删除用户属性（{{ @linkGen.toLink(#userAttrId, T(ResourceType).USER_ATTR) }}）失败"
    )
    @Transactional
    @Override
    public void removeUserAttr(String userAttrId) {
        // 1. 删除用户属性
        super.removeById(userAttrId);

        // 2. 删除关联的用户的属性
        userAttrMappingService.remove(Wrappers.<UserAttrMapping>lambdaQuery().eq(UserAttrMapping::getAttrId, userAttrId));
    }

    /**
     * 获取用户中心可见的用户属性
     *
     * @return 用户中心可见的用户属性
     */
    @Override
    public List<UserAttrResponseDto> getVisibleUserAttrs() {
        // 1. 数据库操作
        List<UserAttr> userAttrs = super.list(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getUserVisible, true).orderByAsc(UserAttr::getAttrKey));

        // 2. 属性编辑
        return CommonUtil.stream(userAttrs).map(userAttr -> {
            UserAttrResponseDto userAttrResponse = new UserAttrResponseDto();
            userAttrResponse.setId(userAttr.getAttrId());
            userAttrResponse.setKey(userAttr.getAttrKey());
            userAttrResponse.setName(userAttr.getAttrName());
            userAttrResponse.setDataType(userAttr.getAttrDataType());
            userAttrResponse.setExtFlg(userAttr.getExtAttrFlg());
            userAttrResponse.setUserEditable(userAttr.getUserEditable());
            userAttrResponse.setDictId(userAttr.getDictId());

            // 2.1 属性数据类型为 DICT，设置是否是级联字典
            if (userAttr.getDictId() != null) {
                userAttrResponse.setCascadeDict(dictService.hasChildDict(userAttr.getDictId()));
            }

            return userAttrResponse;
        }).toList();
    }

    /**
     * 获取数据库中不存在的用户属性
     */
    private List<UserAttrMappingRequestDto> getNotExistAttributes(List<UserAttrMappingRequestDto> attributes, List<UserAttrMapping> mappings) {
        return attributes.stream()
                .filter(a -> mappings.stream().noneMatch(m -> StringUtils.equals(a.getAttrId(), m.getAttrId())))
                .toList();
    }

    private void checkAttrValueForDict(List<UserAttrMappingRequestDto> attributes) {
        if (CollectionUtils.isEmpty(attributes)) {
            return;
        }

        // 1. 获取用户属性数据类型为 DICT 的属性 ID 集合
        var userAttrIds = CommonUtil.stream(super.list(Wrappers.<UserAttr>lambdaQuery().eq(UserAttr::getAttrDataType, UserAttrDataTypeEnum.DICT.getType())))
                .map(UserAttr::getAttrId).toList();
        // 1.1 无 DICT 类型的用户属性
        if (CollectionUtils.isEmpty(userAttrIds)) {
            return;
        }

        // 2. 判断 DICT 类型的用户属性值是否有效
        // 2.1 获取 DICT 类型的用户属性值
        var dictUserAttrValues = attributes.stream().filter(a -> userAttrIds.contains(a.getAttrId())).map(UserAttrMappingRequestDto::getAttrValue).toList();
        if (CollectionUtils.isNotEmpty(dictUserAttrValues)) {
            // 2.2 获取启用的字典数据ID集合
            var enabledDictDataIds = CommonUtil.stream(dictDataService.list(Wrappers.<DictData>lambdaQuery().in(DictData::getDataId, dictUserAttrValues).and(q -> q.eq(DictData::getEnable, true))))
                    .map(DictData::getDataId).toList();
            // 2.3 删除无效的用户和用户属性的映射关系请求
            attributes.removeIf(a -> dictUserAttrValues.contains(a.getAttrValue()) && !enabledDictDataIds.contains(a.getAttrValue()));
        }
    }
}
