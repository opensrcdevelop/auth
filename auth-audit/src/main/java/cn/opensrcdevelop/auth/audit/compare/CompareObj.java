package cn.opensrcdevelop.auth.audit.compare;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import lombok.Builder;
import lombok.Getter;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.javers.core.Javers;
import org.javers.core.JaversBuilder;
import org.javers.core.diff.Change;
import org.javers.core.diff.Diff;
import org.javers.core.diff.ListCompareAlgorithm;
import org.javers.core.diff.changetype.ValueChange;
import org.javers.core.diff.changetype.map.*;

import java.lang.reflect.Field;
import java.util.*;

@Builder
@Getter
public class CompareObj<T> {

    private String entityName;

    private String id;

    private T before;

    private T after;

    @Builder.Default
    private Map<String, String> propertyNames = new HashMap<>();

    @Builder.Default
    private List<String> excludeProperty = new ArrayList<>();

    /**
     * 获取变更文本
     *
     * @return 变更文本
     */
    public String getChangText() {

        // 1. 比较对象
        Javers javers = JaversBuilder.javers()
                .withListCompareAlgorithm(ListCompareAlgorithm.LEVENSHTEIN_DISTANCE)
                .withInitialChanges(true)
                .build();
        Diff diff = javers.compare(before, after);
        var changes = editChanges(diff.getChanges(change -> change instanceof ValueChange || change instanceof MapChange));

        // 2. 获取实体名称
        String eName;
        Class<?> objClass = after.getClass();
        EntityName entityNameAnnotation = objClass.getAnnotation(EntityName.class);
        if (Objects.isNull(entityNameAnnotation)) {
            eName = entityName;
        } else {
            eName = entityNameAnnotation.value();
        }

        // 3. 判断是否存在变更
        if (CollectionUtils.isNotEmpty(changes)) {
            // 3.1 存在变更
            // 3.1.1 构建变更文本
            StringBuilder changeText = new StringBuilder();

            if (Objects.nonNull(id)) {
                changeText.append(String.format("%s（%s）中发生了以下变更：%n", eName, id));
            } else {
                changeText.append(String.format("%s中发生了以下变更：%n", eName));
            }

            // 3.1.2 构建属性显示名称
            Map<String, String> names = buildPropertyNames(objClass);
            names.putAll(propertyNames);
            for (PropertyValueChange change : changes) {
                String propertyName = names.get(change.getPropertyName());
                if (StringUtils.isNotEmpty(propertyName) && !excludeProperty.contains(propertyName)) {
                    changeText.append(String.format("\t%s: %s -> %s %n", propertyName, change.getLeft(), change.getRight()));
                }
            }

            return changeText.toString();
        }

        // 3.2 不存在变更
        if (Objects.nonNull(id)) {
            return String.format("%s（%s）中无任何变更", eName, id);
        } else {
            return String.format("%s中无任何变更", eName);
        }
    }

    /**
     * 获取 java 类型
     *
     * @return java 类型
     */
    public String getJavaType() {
        if (before != null) {
            return before.getClass().getName();
        } else if (after != null) {
            return after.getClass().getName();
        } else {
            return StringUtils.EMPTY;
        }
    }

    private Map<String, String> buildPropertyNames(Class<?> clazz) {
        Map<String, String> names = new HashMap<>();
        for (Field field : clazz.getDeclaredFields()) {
            PropertyName annotation = field.getAnnotation(PropertyName.class);
            if (annotation != null) {
                names.put(field.getName(), annotation.value());
            }
        }

        return names;
    }

    public List<PropertyValueChange> editChanges(List<Change> changes) {
        if (CollectionUtils.isEmpty(changes)) {
            return Collections.emptyList();
        }

        List<PropertyValueChange> newChanges = new ArrayList<>();
        for (Change change : changes) {
            if (change instanceof ValueChange valueChange) {
                newChanges.add(PropertyValueChange.builder()
                        .propertyName(valueChange.getPropertyName())
                        .left(valueChange.getLeft())
                        .right(valueChange.getRight()).build());
            }

            if (change instanceof MapChange<?> mapChange) {
                for (EntryChange entryChange : mapChange.getEntryChanges()) {
                    if (entryChange instanceof EntryValueChange entryValueChange) {
                        newChanges.add(
                                PropertyValueChange.builder()
                                        .propertyName((String) entryValueChange.getKey())
                                        .left(entryValueChange.getLeftValue())
                                        .right(entryValueChange.getRightValue())
                                        .build()
                        );
                    }

                    if (entryChange instanceof EntryAdded entryAdded) {
                        newChanges.add(
                                PropertyValueChange.builder()
                                        .propertyName((String) entryAdded.getKey())
                                        .left(null)
                                        .right(entryAdded.getValue())
                                        .build()
                        );
                    }

                    if (entryChange instanceof EntryRemoved entryRemoved) {
                        newChanges.add(
                                PropertyValueChange.builder()
                                       .propertyName((String) entryRemoved.getKey())
                                       .left(entryRemoved.getValue())
                                       .right(null)
                                       .build()
                        );
                    }
                }
            }
        }

        return newChanges;
    }
}
