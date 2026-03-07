package cn.opensrcdevelop.ai.service;

import java.util.List;

public interface EmbeddingService {

    /**
     * 获取文本的嵌入向量
     *
     * @param text 文本
     * @return 向量列表
     */
    List<Float> embedText(String text);

    /**
     * 获取嵌入模型的维度
     *
     * @return 向量维度
     */
    int getDimension();
}
