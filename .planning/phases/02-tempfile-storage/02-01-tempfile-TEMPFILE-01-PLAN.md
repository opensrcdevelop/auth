---
phase: 02-tempfile-storage
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java
autonomous: true
requirements: [TEMPFILE-01]
must_haves:
  truths:
    - "SQL 查询结果超过阈值时，结果写入临时目录的 .txt 文件"
    - "临时文件每行存储一个 JSON 对象（JSON Lines 格式）"
    - "临时文件存储路径可配置"
  artifacts:
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java"
      provides: "临时文件写入和路径管理"
      exports: ["writeQueryDataToTempFile", "getTempFilePath", "deleteTempFile"]
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java"
      provides: "存储临时文件路径"
      contains: "tempFilePath"
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java"
      provides: "SQL 执行结果超过阈值时写入临时文件"
      contains: "TempFileManager"
  key_links:
    - from: "ExecuteSqlTool.execute()"
      to: "TempFileManager.writeQueryDataToTempFile()"
      via: "threshold check after SQL execution"
      pattern: "if.*size.*>.*threshold"
    - from: "ChatContext"
      to: "TempFileManager"
      via: "tempFilePath field stores file path"
---

<objective>
实现 SQL 查询结果的临时文件存储功能，当结果超过配置阈值时写入 JSON Lines 格式的 .txt 文件。

Purpose: 避免大数据集占用内存，保持 SSE 连接稳定
Output: TempFileManager 类、ChatContext.tempFilePath 字段、ExecuteSqlTool 集成
</objective>

<context>
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
</context>

<tasks>

<task type="auto">
  <name>Task 1: 添加 ChatContext.tempFilePath 字段</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java</files>
  <action>
    在 ChatContext 类中添加 `private String tempFilePath;` 字段及其 getter/setter。
    此字段存储当前会话的临时文件路径，供后续读取和删除使用。
  </action>
  <verify>
    <automated>grep -n "tempFilePath" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java</automated>
  </verify>
  <done>ChatContext 有 tempFilePath 字段和 getter/setter</done>
</task>

<task type="auto">
  <name>Task 2: 创建 TempFileManager 类</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java</files>
  <action>
    创建 TempFileManager 类，提供以下功能：

    1. 配置支持：
       - `tempfile.threshold` 配置项，默认 100
       - `tempfile.directory` 配置项，默认使用系统临时目录

    2. 核心方法：
       - `writeQueryDataToTempFile(List<Map<String, Object>> data, String chatId)`:
         - 检查数据条数是否超过阈值
         - 超过时，将每条数据转为 JSON 对象写入 .txt 文件（JSON Lines 格式）
         - 文件命名：`chatbi_{chatId}_{timestamp}.txt`
         - 返回临时文件路径

       - `getTempFilePath(String chatId)`: 获取指定会话的临时文件路径

       - `deleteTempFile(String filePath)`: 删除指定临时文件

    3. JSON 序列化：使用 Jackson 或 FastJSON 将 Map 转为 JSON 行

    注意：使用 @Service 或 @Component 注册为 Spring Bean
  </action>
  <verify>
    <automated>grep -n "class TempFileManager" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java && grep -n "writeQueryDataToTempFile\|getTempFilePath\|deleteTempFile" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java</automated>
  </verify>
  <done>TempFileManager 包含 writeQueryDataToTempFile、getTempFilePath、deleteTempFile 方法</done>
</task>

<task type="auto">
  <name>Task 3: 集成 TempFileManager 到 ExecuteSqlTool</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java</files>
  <action>
    修改 ExecuteSqlTool.execute() 方法：

    1. 注入 TempFileManager

    2. SQL 执行成功后，检查结果条数：
       - 如果结果条数 <= 阈值：直接返回结果（现有逻辑）
       - 如果结果条数 > 阈值：
         - 调用 tempFileManager.writeQueryDataToTempFile(result._2, chatContext.getChatId())
         - 将返回的文件路径设置到 chatContext.setTempFilePath()
         - 在 Response.queryData 中返回 null 或摘要信息（如 "数据已存储到临时文件，共 {count} 条"）
         - 在 Response 中添加 tempFilePath 和 recordCount 字段

    3. 修改 Response 类，添加：
       - `private String tempFilePath;`
       - `private Integer recordCount;`

    注意：使用构造器注入 TempFileManager
  </action>
  <verify>
    <automated>grep -n "TempFileManager\|tempFilePath\|tempfile.threshold" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java</automated>
  </verify>
  <done>ExecuteSqlTool 集成 TempFileManager，结果超过阈值时写入临时文件</done>
</task>

</tasks>

<verification>
1. 编译成功：`./gradlew :ai-chatbi:compileJava`
2. ExecuteSqlTool 包含 TempFileManager 引用
3. ChatContext 包含 tempFilePath 字段
4. TempFileManager 包含 writeQueryDataToTempFile、getTempFilePath、deleteTempFile 方法
</verification>

<success_criteria>
- SQL 查询结果条数 <= 阈值时，返回正常结果
- SQL 查询结果条数 > 阈值时，结果写入临时 .txt 文件（JSON Lines 格式）
- 临时文件路径存储在 ChatContext.tempFilePath 中
- 临时文件存储路径和阈值可通过配置修改
</success_criteria>

<output>
创建 `.planning/phases/02-tempfile-storage/02-01-tempfile-TEMPFILE-01-SUMMARY.md`
</output>
