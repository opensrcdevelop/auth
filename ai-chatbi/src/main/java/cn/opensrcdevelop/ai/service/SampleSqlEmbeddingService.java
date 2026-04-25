package cn.opensrcdevelop.ai.service;

import java.util.List;

public interface SampleSqlEmbeddingService {

    List<Float> embedText(String text);
}
