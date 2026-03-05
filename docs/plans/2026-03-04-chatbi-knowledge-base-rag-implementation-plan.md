# ChatBI ask_user tool 实现计划

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 实现 ChatBI 模块的 ask_user tool 功能，让 AI 能够在需要时向用户询问缺失信息

**Architecture:** 在现有的 ChatBI Agent 框架中添加 Tool 接口，实现 ask_user tool，支持多种交互类型

**Tech Stack:** Spring Boot 3.5, Spring AI, Vue 3, TypeScript

---

## 阶段 1：ask_user tool 实现计划

### Task 1: 创建问题类型枚举

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/enums/QuestionType.java`

**Step 1: 创建 QuestionType 枚举**

```java
package cn.opensrcdevelop.ai.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum QuestionType {

    TEXT("TEXT", "文本输入"),
    SELECT("SELECT", "单选（支持自定义输入）"),
    MULTI_SELECT("MULTI_SELECT", "多选"),
    DATE("DATE", "日期选择"),
    NUMBER("NUMBER", "数字输入");

    private final String code;
    private final String description;
}
```

**Step 2: 运行编译检查**

```bash
./gradlew :ai-chatbi:compileJava
```

**Step 3: Commit**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/enums/QuestionType.java
git commit -m "feat(chatbi): 添加问题类型枚举

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 2: 实现 ask_user tool（支持多问题）

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/AskUserTool.java`

**Step 1: 创建 AskUserTool（支持多问题）**

```java
package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.Tool;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.List;

@Component(AskUserTool.TOOL_NAME)
@RequiredArgsConstructor
public class AskUserTool implements MethodTool {

    public static final String TOOL_NAME = "ask_user";

    @Tool(name = TOOL_NAME, description = "当 AI 无法直接回答问题或缺少必要信息时，向用户提问获取更多信息。适用于：1. 缺少关键筛选条件；2. 用户意图不明确；3. 需要用户从多个选项中选择（支持自定义输入）。支持同时传递多个问题，用户可通过 tab 切换不同问题。")
    public Response execute(@ToolParam(description = "请求参数") Request request) {
        Response response = new Response();
        response.setSuccess(true);
        response.setQuestions(request.getQuestions());
        response.setIsAskUser(true);
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "问题列表，支持同时传递多个问题（使用 tab 切换）", required = true)
        private List<Question> questions;
    }

    @Data
    public static class Question {

        @ToolParam(description = "问题 ID，用于关联用户回答", required = true)
        private String id;

        @ToolParam(description = "需要询问的问题", required = true)
        private String question;

        @ToolParam(description = "问题类型：TEXT（文本输入）、SELECT（单选，支持自定义输入）、MULTI_SELECT（多选）、DATE（日期选择）、NUMBER（数字输入）", required = false)
        private String questionType;

        @ToolParam(description = "选项列表，当 questionType 为 SELECT 或 MULTI_SELECT 时必填", required = false)
        private List<String> options;

        @ToolParam(description = "是否必填，默认为 true", required = false)
        private Boolean required;

        @ToolParam(description = "上下文信息，帮助用户理解问题", required = false)
        private String context;

        @ToolParam(description = "问题标题（简短）", required = false)
        private String title;
    }

    @Data
    public static class Response {

        @ToolParam(description = "是否成功")
        private Boolean success;

        @ToolParam(description = "问题列表")
        private List<Question> questions;

        @ToolParam(description = "标记需要向用户询问")
        private Boolean isAskUser;
    }
}
```

**Step 2: 运行编译检查**

```bash
./gradlew :ai-chatbi:compileJava
```

**Step 3: Commit**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/AskUserTool.java
git commit -m "feat(chatbi): 实现 ask_user tool（支持多问题）

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 3: 修改 ChatContext 添加等待状态

**Files:**
- Modify: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java`

**Step 1: 添加等待状态相关字段**

在 ChatContext 中添加：
```java
// 等待用户输入的问题
private Map<String, Object> pendingQuestion;

// 等待用户输入的上下文
private Map<String, Object> askUserContext;
```

**Step 2: 添加相关方法**

```java
/**
 * 设置等待用户输入
 * @param question 待回答的问题
 */
public void setWaitingForUser(Map<String, Object> question) {
    this.pendingQuestion = question;
    this.askUserContext = question;
}

/**
 * 清除等待状态
 */
public void clearWaitingState() {
    this.pendingQuestion = null;
    this.askUserContext = null;
}

/**
 * 是否在等待用户输入
 */
public boolean isWaitingForUser() {
    return this.pendingQuestion != null;
}
```

**Step 3: Commit**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
git commit -m "feat(chatbi): 修改 ChatContext 添加等待状态

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 4: 创建对话状态枚举

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/UserResponseRequestDto.java`

**Step 1: 创建 UserResponseRequestDto**

```java
package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "用户响应请求")
@Data
public class UserResponseRequestDto {

    @Schema(description = "对话ID")
    @NotBlank
    private String chatId;

    @Schema(description = "用户回答")
    @NotBlank
    private String answer;

    @Schema(description = "问题ID（用于关联具体问题）")
    private String questionId;
}
```

**Step 2: Commit**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/UserResponseRequestDto.java
git commit -m "feat(chatbi): 添加用户响应 DTO

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 8: 修改 ChatBIService 集成 ask_user

**Files:**
- Modify: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java`

**Step 1: 修改对话处理流程**

1. 在 Agent 处理用户问题前，检查是否在等待用户回答
2. 如果触发了 ask_user tool，设置等待状态，返回给前端
3. 添加用户响应处理接口，继续对话流程

**Step 2: 添加处理用户响应的方法**

```java
/**
 * 处理用户对问题的回答
 * @param request 用户响应
 * @return 继续对话的结果
 */
public Mono<ChatAnswerResponseDto> handleUserResponse(UserResponseRequestDto request) {
    // 1. 获取对话上下文
    ChatContext context = chatContextHolder.getContext();

    // 2. 检查是否在等待用户回答
    if (!context.isWaitingForUser()) {
        throw new BizException("当前没有等待用户回答的问题");
    }

    // 3. 获取之前的上下文和问题
    Map<String, Object> pendingQuestion = context.getPendingQuestion();

    // 4. 将用户回答添加到上下文，继续执行
    // ... 调用 Agent 继续处理

    // 5. 清除等待状态
    context.clearWaitingState();

    // ... 返回结果
}
```

**Step 3: Commit**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
git commit -m "feat(chatbi): 修改 ChatBIService 集成 ask_user

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 9: 添加用户响应 Controller

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/controller/UserResponseController.java`

**Step 1: 创建 UserResponseController**

```java
package cn.opensrcdevelop.ai.controller;

import cn.opensrcdevelop.ai.dto.UserResponseRequestDto;
import cn.opensrcdevelop.ai.service.ChatBIService;
import cn.opensrcdevelop.common.response.R;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/v1/ai/chat")
@RequiredArgsConstructor
public class UserResponseController {

    private final ChatBIService chatBIService;

    /**
     * 处理用户对问题的回答
     */
    @PostMapping("/user-response")
    public R<Void> handleUserResponse(@Valid @RequestBody UserResponseRequestDto request) {
        chatBIService.handleUserResponse(request);
        return R.ok();
    }
}
```

**Step 2: Commit**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/controller/UserResponseController.java
git commit -m "feat(chatbi): 添加用户响应 Controller

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 10: 前端 - 用户响应组件

**Files:**
- Create: `ui/src/components/ai/AskUserDialog.vue`

**Step 1: 创建 AskUserDialog 组件**

```vue
<template>
  <a-modal
    :visible="visible"
    :title="question"
    @cancel="handleCancel"
    @ok="handleSubmit"
  >
    <a-form :model="form">
      <!-- 文本输入 -->
      <a-form-item v-if="questionType === 'TEXT'">
        <a-input v-model="form.answer" placeholder="请输入" />
      </a-form-item>

      <!-- 单选 -->
      <a-form-item v-else-if="questionType === 'SELECT'">
        <a-select v-model="form.answer" placeholder="请选择">
          <a-option v-for="opt in options" :key="opt" :value="opt">{{ opt }}</a-option>
        </a-select>
      </a-form-item>

      <!-- 多选 -->
      <a-form-item v-else-if="questionType === 'MULTI_SELECT'">
        <a-select v-model="form.answer" multiple placeholder="请选择">
          <a-option v-for="opt in options" :key="opt" :value="opt">{{ opt }}</a-option>
        </a-select>
      </a-form-item>

      <!-- 日期选择 -->
      <a-form-item v-else-if="questionType === 'DATE'">
        <a-date-picker v-model="form.answer" style="width: 100%" />
      </a-form-item>

      <!-- 数字输入 -->
      <a-form-item v-else-if="questionType === 'NUMBER'">
        <a-input-number v-model="form.answer" :min="min" :max="max" style="width: 100%" />
      </a-form-item>

      <!-- 上下文信息 -->
      <a-alert v-if="context" :message="context" type="info" class="mt-3" />
    </a-form>
  </a-modal>
</template>

<script setup>
import { ref, watch } from 'vue';

const props = defineProps({
  visible: Boolean,
  question: String,
  questionType: {
    type: String,
    default: 'TEXT'
  },
  options: {
    type: Array,
    default: () => []
  },
  required: {
    type: Boolean,
    default: true
  },
  context: String
});

const emit = defineEmits(['submit', 'cancel']);

const form = ref({
  answer: ''
});

const handleSubmit = () => {
  emit('submit', form.value.answer);
  form.value.answer = '';
};

const handleCancel = () => {
  emit('cancel');
  form.value.answer = '';
};
</script>
```

**Step 2: Commit**

```bash
git add ui/src/components/ai/AskUserDialog.vue
git commit -m "feat(chatbi): 添加用户响应前端组件

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 11: 运行完整编译和测试

**Step 1: 运行编译**

```bash
./gradlew clean build -x test
```

**Step 2: 运行测试**

```bash
./gradlew test
```

**Step 3: Commit**

```bash
git add .
git commit -m "feat(chatbi): 完成 ask_user tool 功能开发

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## 阶段 2：知识库 RAG 实现计划

（待续...）
