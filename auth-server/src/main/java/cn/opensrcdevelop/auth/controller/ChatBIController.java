package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.DataSourceConfResponseDto;
import cn.opensrcdevelop.ai.dto.ModelProviderResponseDto;
import cn.opensrcdevelop.ai.dto.VoteChartRequestDto;
import cn.opensrcdevelop.ai.service.ChatBIService;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.response.PageData;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

@Tag(name = "Chat BI", description = "接口-Chat BI")
@RestController
@RestResponse
@RequestMapping("/chatbi")
@RequiredArgsConstructor
public class ChatBIController {

    private final ChatBIService chatBIService;
    private final DataSourceConfService dataSourceConfService;
    private final ModelProviderService modelProviderService;

    @Operation(summary = "流式对话", description = "流式对话")
    @PostMapping(path = "/chat/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter streamChatBI(@RequestBody ChatBIRequestDto requestDto) {
        return chatBIService.streamChatBI(requestDto);
    }

    @Operation(summary = "数据分析", description = "数据分析")
    @PostMapping(path = "/analyze/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter streamAnalyzeData(@RequestBody ChatBIRequestDto requestDto) {
        return chatBIService.streamAnalyzeData(requestDto);
    }

    @Operation(summary = "获取数据源配置列表", description = "获取数据源配置列表")
    @GetMapping("/dataSourceConf/list")
    public PageData<DataSourceConfResponseDto> listDataSourceConf(@RequestParam(required = false) String keyword, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size) {
        return dataSourceConfService.list(keyword, page, size);
    }

    @Operation(summary = "获取模型提供商列表", description = "获取模型提供商列表")
    @GetMapping("/modelProvider/list")
    public PageData<ModelProviderResponseDto> listModelProvider(@RequestParam(required = false) String keyword, @RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "15") int size) {
        return modelProviderService.list(keyword, page, size);
    }

    @Operation(summary = "投票图表", description = "投票图表")
    @PostMapping("/chart/vote")
    public void voteChart(@RequestBody @Valid VoteChartRequestDto requestDto) {
        chatBIService.voteChart(requestDto);
    }
}
