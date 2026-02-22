---
description: 加载项目规则文件到上下文
parameters:
  - name: files
    description: 规则文件名（不含.md扩展名），多个文件用逗号分隔。留空则加载所有规则。
    type: string
    default: ""
---

请按以下步骤加载规则文件：

1. 首先列出 `.claude/rules/` 目录下所有可用的规则文件：
   ```bash
   ls -1 .claude/rules/
   ```

2. 根据用户指定的参数 {{files}} 决定加载范围：
   - 如果 {{files}} 为空或未指定：加载所有 `.md` 规则文件
   - 如果 {{files}} 有值：仅加载指定的文件（支持逗号分隔的多个文件名）

3. 使用 Read 工具读取每个规则文件的完整内容

4. 向用户报告已加载的规则文件列表和简要概述

{{cursor}}
