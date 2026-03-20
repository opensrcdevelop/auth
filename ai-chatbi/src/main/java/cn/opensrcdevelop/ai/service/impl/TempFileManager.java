package cn.opensrcdevelop.ai.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class TempFileManager {

    private static final String FILE_PREFIX = "chatbi_";
    private static final String FILE_SUFFIX = ".txt";

    @Value("${tempfile.threshold:100}")
    private int threshold;

    @Value("${tempfile.directory:#{systemProperties['java.io.tmpdir']}}")
    private String directory;

    private final ObjectMapper objectMapper;

    public TempFileManager() {
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
            log.info("已将 {} 条数据写入临时文件: {}", data.size(), filePath);
            return filePath.toString();
        } catch (IOException e) {
            log.error("写入临时文件失败: {}", filePath, e);
            return null;
        }
    }

    /**
     * 获取指定会话的临时文件路径
     *
     * @param chatId
     *            会话 ID
     * @return 临时文件路径，如果不存在则返回 null
     */
    public String getTempFilePath(String chatId) {
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
            return null;
        }

        Path path = Paths.get(filePath);
        if (!Files.exists(path)) {
            log.warn("临时文件不存在: {}", filePath);
            return null;
        }

        List<Map<String, Object>> result = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(path.toFile()))) {
            // 跳过前 offset 行
            for (int i = 0; i < offset; i++) {
                if (reader.readLine() == null) {
                    break;
                }
            }

            // 读取最多 limit 行
            String line;
            int count = 0;
            while (count < limit && (line = reader.readLine()) != null) {
                try {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> row = objectMapper.readValue(line, Map.class);
                    result.add(row);
                    count++;
                } catch (IOException e) {
                    log.warn("解析 JSON 行失败: {}", line, e);
                }
            }
        } catch (IOException e) {
            log.error("读取临时文件失败: {}", filePath, e);
            return null;
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
     * 获取阈值配置
     */
    public int getThreshold() {
        return threshold;
    }
}
