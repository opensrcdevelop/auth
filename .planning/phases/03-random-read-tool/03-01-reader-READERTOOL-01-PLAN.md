---
phase: 03-random-reader
plan: "01"
type: execute
wave: 1
depends_on: []
files_modified:
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
autonomous: true
requirements:
  - READERTOOL-01
must_haves:
  truths:
    - "AI can call read_query_result Tool with offset and limit parameters"
    - "Tool returns JSON Lines data from the specified offset position"
    - "Tool returns error when temp file does not exist"
  artifacts:
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java"
      provides: "Tool implementation"
      min_lines: 60
  key_links:
    - from: "ReadQueryResultTool"
      to: "TempFileManager"
      via: "readLinesFromTempFile()"
      pattern: "tempFileManager\\.readLinesFromTempFile"
---

<objective>
创建 `read_query_result` Tool，支持 AI 按偏移量读取临时文件中的查询结果。

Purpose: 让 AI 能够分批获取大数据量查询结果，继续分析而不受内存限制
Output: ReadQueryResultTool.java + TempFileManager.readLinesFromTempFile()
</objective>

<execution_context>
@/Users/lee0407/dev/projs/auth/worktrees/feature-chatbi-heartbeat-tempfile/ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/MethodTool.java
@/Users/lee0407/dev/projs/auth/worktrees/feature-chatbi-heartbeat-tempfile/ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java
@/Users/lee0407/dev/projs/auth/worktrees/feature-chatbi-heartbeat-tempfile/ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
</execution_context>

<tasks>

<task type="auto">
  <name>Task 1: 添加 readLinesFromTempFile 方法到 TempFileManager</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java</files>
  <action>
在 TempFileManager 中添加 `readLinesFromTempFile(String filePath, int offset, int limit)` 方法：

1. 使用 BufferedReader 按行读取文件
2. 跳过前 offset 行
3. 最多读取 limit 行
4. 每行作为独立 JSON 解析后装入 List<Map<String, Object>> 返回
5. 文件不存在时返回 null
6. 使用 try-with-resources 保证资源关闭

示例签名：
```java
public List<Map<String, Object>> readLinesFromTempFile(String filePath, int offset, int limit)
```
</action>
  <verify>
  <automated>grep -n "readLinesFromTempFile" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java</automated>
  </verify>
  <done>readLinesFromTempFile 方法已添加，支持按 offset/limit 读取 JSON Lines</done>
</task>

<task type="auto">
  <name>Task 2: 创建 ReadQueryResultTool</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java</files>
  <action>
创建 ReadQueryResultTool，实现 MethodTool 接口：

1. 使用 `@Component(ReadQueryResultTool.TOOL_NAME)` 注册
2. 使用 `@Tool(name = "read_query_result", description = "...")` 注解
3. 注入 TempFileManager
4. 从 ChatContextHolder 获取当前会话的 tempFilePath
5. 调用 tempFileManager.readLinesFromTempFile(path, offset, limit) 获取数据
6. 返回 Response（包含 queryData、recordCount、hasMore）

Request 参数：
- offset: int - 起始偏移量
- limit: int - 要读取的条数

Response 字段：
- queryData: List<Map<String, Object>> - 读取的数据
- recordCount: int - 本次返回的条数
- hasMore: boolean - 是否还有更多数据
- error: string - 错误信息（如有）
</action>
  <verify>
  <automated>grep -n "class ReadQueryResultTool" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java</automated>
  </verify>
  <done>ReadQueryResultTool 已创建，Tool 名称为 read_query_result，可被 AI 调用</done>
</task>

</tasks>

<verification>
编译检查：
```bash
./gradlew :ai-chatbi:compileJava 2>&1 | grep -E "(error|BUILD|success)"
```

验证 Tool 被 Spring 容器扫描到：
```bash
grep -r "read_query_result" ai-chatbi/src/main/java/ --include="*.java" | head -5
```
</verification>

<success_criteria>
1. ReadQueryResultTool 类存在，实现 MethodTool 接口
2. Tool 名称为 read_query_result
3. TempFileManager.readLinesFromTempFile 方法可按 offset/limit 读取
4. 编译通过
</success_criteria>

<output>
完成创建 `.planning/phases/03-random-reader/03-01-reader-READERTOOL-01-SUMMARY.md`
</output>
