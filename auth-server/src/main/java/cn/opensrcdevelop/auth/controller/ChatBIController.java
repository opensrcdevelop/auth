package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.ai.dto.*;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.auth.client.authorize.annoation.Authorize;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.List;

@Tag(name = "API-Chat BI", description = "接口-Chat BI")
@RestController
@RestResponse
@RequestMapping("/chatbi")
@RequiredArgsConstructor
public class ChatBIController {

    private final ChatBIService chatBIService;
    private final DataSourceConfService dataSourceConfService;
    private final ModelProviderService modelProviderService;
    private final TableService tableService;
    private final TableFieldService tableFieldService;
    private final ChatHistoryService chatHistoryService;
    private final ChatMessageHistoryService chatMessageHistoryService;

    @Operation(summary = "流式对话", description = "流式对话")
    @PostMapping(path = "/chat/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    @Authorize({ "allChatBIPermissions", "chat" })
    public SseEmitter streamChatBI(@RequestBody ChatBIRequestDto requestDto) {
        return chatBIService.streamChatBI(requestDto);
    }

    @Operation(summary = "获取已启用的数据源配置", description = "获取已启用的数据源配置")
    @GetMapping("/dataSourceConf/enabled")
    public List<DataSourceConfResponseDto> enabledDataSourceConf() {
        return dataSourceConfService.enabledList();
    }

    @Operation(summary = "获取数据源配置列表", description = "获取数据源配置列表")
    @Parameters({
            @Parameter(name = "keyword", description = "数据源名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @GetMapping("/dataSourceConf/list")
    @Authorize({ "allChatBIDataSourcePermissions", "getDataSourceConfList" })
    public PageData<DataSourceConfResponseDto> listDataSourceConf(@RequestParam(required = false) String keyword, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size) {
        return dataSourceConfService.list(keyword, page, size);
    }

    @Operation(summary = "获取数据源下的表列表", description = "获取数据源下的表列表")
    @Parameters({
            @Parameter(name = "id", description = "数据源ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "keyword", description = "表名检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @GetMapping("/dataSourceConf/{id}/table/list")
    @Authorize({ "allChatBIDataSourcePermissions", "getDataSourceTableList" })
    public PageData<TableResponseDto> listTable(@PathVariable @NotBlank String id,
                                                @RequestParam(required = false) String keyword,
                                                @RequestParam(defaultValue = "1") int page,
                                                @RequestParam(defaultValue = "15") int size) {
        return tableService.list(id, keyword, page, size);
    }

    @Operation(summary = "获取表字段列表", description = "获取表字段列表")
    @Parameters({
            @Parameter(name = "id", description = "表ID", in = ParameterIn.PATH, required = true),
            @Parameter(name = "keyword", description = "字段名检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @GetMapping("/table/{id}/field/list")
    @Authorize({ "allChatBIDataSourcePermissions", "getDataSourceTableFieldList" })
    public PageData<TableFieldResponseDto> listTableField(@PathVariable @NotBlank String id,
                                                          @RequestParam(required = false) String keyword,
                                                          @RequestParam(defaultValue = "1") int page,
                                                          @RequestParam(defaultValue = "15") int size) {
        return tableFieldService.list(id, keyword, page, size);
    }

    @Operation(summary = "获取已启用的模型提供商列表", description = "获取已启用的模型提供商列表")
    @GetMapping("/modelProvider/enabled")
    public List<ModelProviderResponseDto> enabledModelProvider() {
        return modelProviderService.enabledList();
    }

    @Operation(summary = "获取模型提供商列表", description = "获取模型提供商列表")
    @Parameters({
            @Parameter(name = "keyword", description = "模型提供商名称检索关键字", in = ParameterIn.QUERY),
            @Parameter(name = "page", description = "页数", in = ParameterIn.QUERY, required = true),
            @Parameter(name = "size", description = "条数", in = ParameterIn.QUERY, required = true)
    })
    @GetMapping("/modelProvider/list")
    @Authorize({ "allChatBIModelProviderPermissions", "getModelProviderList" })
    public PageData<ModelProviderResponseDto> listModelProvider(@RequestParam(required = false) String keyword, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size) {
        return modelProviderService.list(keyword, page, size);
    }

    @Operation(summary = "投票回答", description = "投票回答")
    @PostMapping("/answer/vote")
    public void voteAnswer(@RequestBody @Valid VoteAnswerRequestDto requestDto) {
        chatBIService.voteAnswer(requestDto);
    }

    @Operation(summary = "获取数据源配置详情", description = "获取数据源配置详情")
    @Parameters({
            @Parameter(name = "id", description = "数据源ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/dataSourceConf/{id}")
    @Authorize({ "allChatBIDataSourcePermissions", "getDataSourceConfDetail" })
    public DataSourceConfResponseDto dataSourceConfDetail(@PathVariable @NotBlank String id) {
        return dataSourceConfService.detail(id);
    }

    @Operation(summary = "同步数据表", description = "同步数据表")
    @Parameters({
            @Parameter(name = "id", description = "数据源ID", in = ParameterIn.PATH, required = true)
    })
    @PostMapping("/dataSourceConf/{id}/syncTable")
    @Authorize({ "allChatBIDataSourcePermissions", "syncDataSourceTable" })
    public void syncTable(@PathVariable @NotBlank String id) {
        dataSourceConfService.syncTable(id);
    }

    @Operation(summary = "创建数据源配置", description = "创建数据源配置")
    @PostMapping("/dataSourceConf")
    @Authorize({ "allChatBIDataSourcePermissions", "createDataSourceConf" })
    public void createDataSourceConf(@RequestBody @Validated(ValidationGroups.Operation.INSERT.class) DataSourceConfRequestDto requestDto) {
        dataSourceConfService.createDataSourceConf(requestDto);
    }

    @Operation(summary = "更新数据源配置", description = "更新数据源配置")
    @PutMapping("/dataSourceConf")
    @Authorize({ "allChatBIDataSourcePermissions", "updateDataSourceConf" })
    public void updateDataSourceConf(@RequestBody @Validated(ValidationGroups.Operation.UPDATE.class) DataSourceConfRequestDto requestDto) {
        dataSourceConfService.updateDataSourceConf(requestDto);
    }

    @Operation(summary = "测试数据源连接", description = "测试数据源连接")
    @PostMapping("/dataSourceConf/testConn")
    @Authorize({ "allChatBIDataSourcePermissions", "testDataSourceConn" })
    public TestDataSourceConnResponseDto testDataSourceConn(@RequestBody @Valid TestDataSourceConnRequestDto requestDto) {
        return dataSourceConfService.testConn(requestDto);
    }

    @Operation(summary = "批量更新表", description = "批量更新表")
    @PutMapping("/table/batchUpdate")
    @Authorize({ "allChatBIDataSourcePermissions", "batchUpdateDataSourceTable" })
    public void batchUpdateTable(@RequestBody @Valid BatchUpdateTableRequestDto requestDto) {
        tableService.batchUpdate(requestDto);
    }

    @Operation(summary = "批量更新表字段", description = "批量更新表字段")
    @PutMapping("/table/field/batchUpdate")
    @Authorize({ "allChatBIDataSourcePermissions", "batchUpdateDataSourceTableField" })
    public void batchUpdateTableField(@RequestBody @Valid BatchUpdateTableFieldRequestDto requestDto) {
        tableFieldService.batchUpdate(requestDto);
    }

    @Operation(summary = "删除数据源配置", description = "删除数据源配置")
    @Parameters({
            @Parameter(name = "id", description = "数据源ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/dataSourceConf/{id}")
    @Authorize({ "allChatBIDataSourcePermissions", "deleteDataSourceConf" })
    public void removeDataSourceConf(@PathVariable @NotBlank String id) {
        dataSourceConfService.removeDataSourceConf(id);
    }

    @Operation(summary = "获取当前用户的对话历史记录", description = "获取当前用户的对话历史记录")
    @Parameters({
            @Parameter(name = "keyword", description = "对话标题检索关键词", in = ParameterIn.QUERY)
    })
    @GetMapping("/chat/history")
    public List<ChatHistoryResponseDto> getUserChatHistory(@RequestParam(required = false) String keyword) {
        return chatHistoryService.listUserChatHistory(keyword);
    }

    @Operation(summary = "获取当前用户的对话消息历史记录", description = "获取当前用户的对话消息历史记录")
    @Parameters({
            @Parameter(name = "id", description = "对话ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/chat/{id}/history")
    public List<ChatMessageHistoryResponseDto> getUserChatMessageHistory(@PathVariable @NotBlank String id) {
        return chatMessageHistoryService.listChatMessageHistory(id);
    }

    @Operation(summary = "删除当前用户的对话历史记录", description = "删除当前用户的对话历史记录")
    @Parameters({
            @Parameter(name = "id", description = "对话ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/chat/{id}")
    public void removeUserChatHistory(@PathVariable @NotBlank String id) {
        chatHistoryService.removeUserChatHistory(id);
    }

    @Operation(summary = "更新当前用户的对话历史记录", description = "更新当前用户的对话历史记录")
    @PutMapping("/chat/history")
    public void updateUserChatHistory(@RequestBody @Valid ChatHistoryRequestDto requestDto) {
        chatHistoryService.updateUserChatHistory(requestDto);
    }

    @Operation(summary = "获取模型提供商详情", description = "获取模型提供商详情")
    @Parameters({
            @Parameter(name = "id", description = "模型提供商ID", in = ParameterIn.PATH, required = true)
    })
    @GetMapping("/modelProvider/{id}")
    @Authorize({ "allChatBIPermissions", "getModelProviderDetail" })
    public ModelProviderResponseDto modelProviderDetail(@PathVariable @NotBlank String id) {
        return modelProviderService.detail(id);
    }

    @Operation(summary = "创建模型提供商", description = "创建模型提供商")
    @PostMapping("/modelProvider")
    @Authorize({ "allChatBIModelProviderPermissions", "createModelProvider" })
    public void createModelProvider(@RequestBody @Validated(ValidationGroups.Operation.INSERT.class) ModelProviderRequestDto requestDto) {
        modelProviderService.createModelProvider(requestDto);
    }

    @Operation(summary = "更新模型提供商", description = "更新模型提供商")
    @PutMapping("/modelProvider")
    @Authorize({ "allChatBIModelProviderPermissions", "updateModelProvider" })
    public void updateModelProvider(@RequestBody @Validated(ValidationGroups.Operation.UPDATE.class) ModelProviderRequestDto requestDto) {
        modelProviderService.updateModelProvider(requestDto);
    }

    @Operation(summary = "删除模型提供商", description = "删除模型提供商")
    @Parameters({
            @Parameter(name = "id", description = "模型提供商ID", in = ParameterIn.PATH, required = true)
    })
    @DeleteMapping("/modelProvider/{id}")
    @Authorize({ "allChatBIModelProviderPermissions", "deleteModelProvider" })
    public void removeModelProvider(@PathVariable @NotBlank String id) {
        modelProviderService.removeModelProvider(id);
    }
}
