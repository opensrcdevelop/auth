package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.entity.MultiChatMemory;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface MultiChatMemoryService extends IService<MultiChatMemory> {

    List<String> getUserHistoryQuestions(String chatId);
}
