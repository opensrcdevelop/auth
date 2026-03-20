---
phase: 04-v1quality-fix
plan: "01"
type: execute
wave: 1
depends_on: []
files_modified:
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/QueryResultTempFileManager.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
  - auth-server/src/main/resources/application-ai.properties
autonomous: true
requirements:
  - QUALITYFIX-01
must_haves:
  truths:
    - "ChatContext.queryData 始终保留完整查询结果，不受 temp 文件存储影响"
    - "TempFileManager 改名为 QueryResultTempFileManager，属性名符合 chatbi.query-result.* 规范"
    - "多次执行 SQL 时，所有 temp 文件路径都被记录到 ChatContext"
    - "会话结束时，所有 temp 文件都被清理"
artifacts:
  - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/QueryResultTempFileManager.java"
    provides: "Renamed class with proper property names"
    min_lines: 60
---

<objective>
修复 v1.0 实现中的质量问题：
1. ExecuteSqlTool 不应将 ChatContext.queryData 设为 null
2. TempFileManager 改名为 QueryResultTempFileManager
3. 属性名从 tempfile.* 改为 chatbi.query-result.*
4. tempFilePath 改为 List<String> 支持多次 SQL
5. 添加 application-ai.properties 配置

修复后：ChatContext.queryData 始终保留完整数据，temp 文件仅用于 ReadQueryResultTool 分页读取。
</objective>

<tasks>

<task type="auto">
  <name>Task 1: 重命名 TempFileManager 为 QueryResultTempFileManager</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java</files>
  <action>
1. 创建新文件 `QueryResultTempFileManager.java`，内容从 TempFileManager 复制
2. 修改类名和注解：`@Service` 保持，但考虑移除或使用新名称
3. 属性重命名：
   - `tempfile.threshold` → `chatbi.query-result.threshold`
   - `tempfile.directory` → `chatbi.query-result.directory`
4. 方法名调整（如需要保持语义清晰）
5. 删除原 TempFileManager.java
6. 更新所有引用：
   - ExecuteSqlTool
   - ReadQueryResultTool
   - ChatBIServiceImpl
</action>
  <verify>
  <automated>grep -n "QueryResultTempFileManager" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/QueryResultTempFileManager.java</automated>
  </verify>
  <done>QueryResultTempFileManager 已创建，属性使用 chatbi.query-result.* 规范</done>
</task>

<task type="auto">
  <name>Task 2: 修改 ChatContext 字段名和类型</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java</files>
  <action>
将 `tempFilePath: String` 改为 `queryResultFilePaths: List<String>`：

1. 添加 import: `import java.util.ArrayList;` 和 `import java.util.List;`
2. 字段修改：
   ```java
   // 旧
   private String tempFilePath;

   // 新
   /** 查询结果临时文件路径列表（支持多次 SQL 执行） */
   private List<String> queryResultFilePaths = new ArrayList<>();
   ```
3. 添加辅助方法：
   ```java
   public void addQueryResultFilePath(String path) {
       if (queryResultFilePaths == null) {
           queryResultFilePaths = new ArrayList<>();
       }
       queryResultFilePaths.add(path);
   }

   public void clearQueryResultFilePaths() {
       if (queryResultFilePaths != null) {
           queryResultFilePaths.clear();
       }
   }
   ```
4. 更新 getter 以支持流式操作（如需要）
</action>
  <verify>
  <automated>grep -n "queryResultFilePaths" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java</automated>
  </verify>
  <done>ChatContext.queryResultFilePaths 已添加，类型为 List<String></done>
</task>

<task type="auto">
  <name>Task 3: 修复 ExecuteSqlTool 不清除 queryData</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java</files>
  <action>
修复 ExecuteSqlTool 中的 bug：不应该将 ChatContext.queryData 设为 null

1. 修改这段代码：
   ```java
   // 旧代码（错误）
   if (tempFilePath != null) {
       chatContext.setTempFilePath(tempFilePath);
       response.setTempFilePath(tempFilePath);
       response.setRecordCount(queryData.size());
       response.setQueryData(null);  // ❌ 不应该设置 null
   }

   // 新代码（正确）
   if (tempFilePath != null) {
       chatContext.addQueryResultFilePath(tempFilePath);  // 添加到列表
       response.setTempFilePath(tempFilePath);
       response.setRecordCount(queryData.size());
       response.setQueryData(null);  // Response 只返回引用，ChatContext.queryData 保持不变
   }
   ```
2. 确保 `chatContext.setQueryData(queryData)` 在第 66 行保留（无论数据是否超阈值）
3. 更新 import：`List` 和 `ArrayList`（如果使用）
</action>
  <verify>
  <automated>grep -n "response.setQueryData(null)" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java</automated>
  </verify>
  <done>ExecuteSqlTool 不再清除 ChatContext.queryData</done>
</task>

<task type="auto">
  <name>Task 4: 更新 ReadQueryResultTool 使用新字段名</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java</files>
  <action>
更新 ReadQueryResultTool 使用新的 QueryResultTempFileManager 和字段名：

1. import 修改：
   ```java
   // 旧
   import cn.opensrcdevelop.ai.service.impl.TempFileManager;

   // 新
   import cn.opensrcdevelop.ai.service.impl.QueryResultTempFileManager;
   ```
2. 字段类型修改：
   ```java
   // 旧
   private final TempFileManager tempFileManager;

   // 新
   private final QueryResultTempFileManager queryResultTempFileManager;
   ```
3. 方法调用修改：
   ```java
   // 旧：获取单个路径
   String tempFilePath = chatContext.getTempFilePath();

   // 新：获取最新的路径（列表最后一个）
   List<String> paths = chatContext.getQueryResultFilePaths();
   String tempFilePath = (paths != null && !paths.isEmpty()) ? paths.get(paths.size() - 1) : null;
   ```
4. 方法名修改：
   ```java
   // 旧
   tempFileManager.readLinesFromTempFile(tempFilePath, offset, limit);

   // 新
   queryResultTempFileManager.readLinesFromTempFile(tempFilePath, offset, limit);
   ```
</action>
  <verify>
  <automated>grep -n "QueryResultTempFileManager" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java</automated>
  </verify>
  <done>ReadQueryResultTool 已更新使用新名称</done>
</task>

<task type="auto">
  <name>Task 5: 更新 ChatBIServiceImpl 清理逻辑</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</files>
  <action>
更新 ChatBIServiceImpl 使用新的字段名和方法：

1. import 修改：
   ```java
   import cn.opensrcdevelop.ai.service.impl.QueryResultTempFileManager;
   ```

2. 注入方式修改：
   ```java
   // 旧
   private final TempFileManager tempFileManager;

   // 新
   private final QueryResultTempFileManager queryResultTempFileManager;
   ```

3. 清理逻辑修改（遍历所有路径）：
   ```java
   // 旧：获取单个路径
   String path = tempFilePathRef.get();

   // 新：获取所有路径并清理
   List<String> paths = chatContext.getQueryResultFilePaths();
   if (paths != null && !paths.isEmpty()) {
       for (String path : paths) {
           queryResultTempFileManager.deleteTempFile(path);
       }
   }
   ```

4. 清理完成后的回调中：
   ```java
   chatContext.clearQueryResultFilePaths();
   ```
</action>
  <verify>
  <automated>grep -n "queryResultTempFileManager" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</automated>
  </verify>
  <done>ChatBIServiceImpl 已更新清理逻辑</done>
</task>

<task type="auto">
  <name>Task 6: 添加 application-ai.properties 配置</name>
  <files>auth-server/src/main/resources/application-ai.properties</files>
  <action>
在 application-ai.properties 中添加配置：

```properties
# ChatBI 查询结果临时文件配置
chatbi.query-result.threshold=100
chatbi.query-result.directory=${java.io.tmpdir}
```
</action>
  <verify>
  <automated>grep -n "chatbi.query-result" auth-server/src/main/resources/application-ai.properties</automated>
  </verify>
  <done>application-ai.properties 已添加 chatbi.query-result.* 配置</done>
</task>

</tasks>

<verification>
编译检查：
```bash
./gradlew :ai-chatbi:compileJava 2>&1 | grep -E "(error|BUILD|success)"
```

检查类重命名：
```bash
grep -rn "TempFileManager" ai-chatbi/src/main/java/ --include="*.java"
# 应该没有结果（已全部改为 QueryResultTempFileManager）
```
</verification>

<success_criteria>
1. TempFileManager 已改名为 QueryResultTempFileManager
2. 所有属性使用 chatbi.query-result.* 规范
3. ChatContext.queryData 始终保留完整数据
4. queryResultFilePaths 类型为 List<String>
5. 所有 temp 文件在会话结束时被清理
6. 编译通过
</success_criteria>
