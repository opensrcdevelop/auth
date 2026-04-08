package cn.opensrcdevelop.ai.component;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Service
@Slf4j
public class QueryResultTempFileManager {

    private static final String FILE_PREFIX = "chatbi_query_result_";
    private static final String FILE_SUFFIX = ".txt";

    @Value("${chatbi.query-result.threshold:100}")
    private int threshold;

    @Value("${chatbi.query-result.directory}")
    private String directory;

    private final ObjectMapper objectMapper;

    public QueryResultTempFileManager() {
        this.objectMapper = new ObjectMapper();
        this.objectMapper.registerModule(new JavaTimeModule());
        this.objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }

    /**
     * 将查询数据写入临时文件（JSON Lines 格式）
     *
     * @param data
     *            查询数据列表
     * @param chatId
     *            会话 ID
     * @return 临时文件路径，如果数据条数未超过阈值则返回 null
     */
    public String writeQueryDataToTempFile(List<Map<String, Object>> data, String chatId) {
        if (data == null || data.isEmpty()) {
            return null;
        }
        if (data.size() <= threshold) {
            return null;
        }

        String fileName = FILE_PREFIX + chatId + "_" + System.currentTimeMillis() + FILE_SUFFIX;
        Path filePath = Paths.get(directory, fileName);

        try {
            Files.createDirectories(Paths.get(directory));
            try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath.toFile()))) {
                for (Map<String, Object> row : data) {
                    String jsonLine = objectMapper.writeValueAsString(row);
                    writer.write(jsonLine);
                    writer.newLine();
                }
            }
            return filePath.toString();
        } catch (IOException e) {
            log.error("写入临时文件失败: {}", filePath, e);
            return null;
        }
    }

    /**
     * 获取指定会话的最新临时文件路径
     *
     * @param chatId
     *            会话 ID
     * @return 临时文件路径，如果不存在则返回 null
     */
    public String getLatestTempFilePath(String chatId) {
        File dir = new File(directory);
        if (!dir.exists() || !dir.isDirectory()) {
            return null;
        }

        File[] files = dir
                .listFiles((d, name) -> name.startsWith(FILE_PREFIX + chatId + "_") && name.endsWith(FILE_SUFFIX));
        if (files == null || files.length == 0) {
            return null;
        }

        // 返回最新的临时文件
        long latestTime = 0;
        String latestPath = null;
        for (File file : files) {
            if (file.lastModified() > latestTime) {
                latestTime = file.lastModified();
                latestPath = file.getAbsolutePath();
            }
        }
        return latestPath;
    }

    /**
     * 获取指定会话的所有临时文件路径
     *
     * @param chatId
     *            会话 ID
     * @return 临时文件路径列表
     */
    public List<String> getAllTempFilePaths(String chatId) {
        List<String> paths = new ArrayList<>();
        File dir = new File(directory);
        if (!dir.exists() || !dir.isDirectory()) {
            return paths;
        }

        File[] files = dir
                .listFiles((d, name) -> name.startsWith(FILE_PREFIX + chatId + "_") && name.endsWith(FILE_SUFFIX));
        if (files == null) {
            return paths;
        }

        for (File file : files) {
            paths.add(file.getAbsolutePath());
        }
        return paths;
    }

    /**
     * 从临时文件读取指定范围的 JSON Lines 数据
     *
     * @param filePath
     *            文件路径
     * @param offset
     *            起始偏移量（跳过的行数）
     * @param limit
     *            最多读取的条数
     * @return 解析后的数据列表，文件不存在或读取失败时返回 null
     */
    public List<Map<String, Object>> readLinesFromTempFile(String filePath, int offset, int limit) {
        if (filePath == null || filePath.isEmpty()) {
            return Collections.emptyList();
        }

        Path path = Paths.get(filePath);
        if (!Files.exists(path)) {
            log.warn("临时文件不存在: {}", filePath);
            return Collections.emptyList();
        }

        List<Map<String, Object>> result = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(path.toFile()))) {
            // 跳过前 offset 行
            for (int i = 0; i < offset; i++) {
                String skippedLine = reader.readLine();
                if (skippedLine == null) {
                    break;
                }
            }

            // 读取最多 limit 行
            String line;
            int count = 0;
            while (count < limit && (line = reader.readLine()) != null) {
                Map<String, Object> row = objectMapper.readValue(line, new TypeReference<Map<String, Object>>() {
                });
                result.add(row);
                count++;
            }
        } catch (IOException e) {
            log.error("读取临时文件失败: {}", filePath, e);
            return Collections.emptyList();
        }

        return result;
    }

    /**
     * 删除指定临时文件
     *
     * @param filePath
     *            文件路径
     * @return 是否删除成功
     */
    public boolean deleteTempFile(String filePath) {
        if (filePath == null || filePath.isEmpty()) {
            return false;
        }

        try {
            Path path = Paths.get(filePath);
            boolean deleted = Files.deleteIfExists(path);
            if (deleted) {
                log.info("已删除临时文件: {}", filePath);
            }
            return deleted;
        } catch (IOException e) {
            log.error("删除临时文件失败: {}", filePath, e);
            return false;
        }
    }

    /**
     * 删除指定会话的所有临时文件
     *
     * @param chatId
     *            会话 ID
     * @return 删除的文件数量
     */
    public int deleteAllTempFiles(String chatId) {
        List<String> paths = getAllTempFilePaths(chatId);
        int deletedCount = 0;
        for (String path : paths) {
            if (deleteTempFile(path)) {
                deletedCount++;
            }
        }
        return deletedCount;
    }
}
