package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlEmbeddingConfigDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;
import java.util.List;

public interface SampleSqlService {

    List<SampleSqlDto> list(String dataSourceId, String question, long offset, int limit);

    long count(String dataSourceId, String question);

    void add(SampleSqlRequestDto requestDto);

    void delete(String id);

    int syncFromLikes();

    void addToVectorStore(String answerId);

    void removeFromVectorStore(String answerId);

    List<SampleSqlDto> search(String dataSourceId, String question, Integer topK);

    SampleSqlEmbeddingConfigDto getEmbeddingConfig();

    void updateEmbeddingConfig(SampleSqlEmbeddingConfigDto configDto);

    boolean needRebuildIndex(SampleSqlEmbeddingConfigDto configDto);

    void rebuildIndex();
}
