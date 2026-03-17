package cn.opensrcdevelop.ai.service;

import java.util.List;

public interface EmbeddingService {

    List<Float> embedText(String text);
}
