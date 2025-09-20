package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.datasource.DataSourceMetaCollector;
import cn.opensrcdevelop.ai.dto.DataSourceConfRequestDto;
import cn.opensrcdevelop.ai.dto.DataSourceConfResponseDto;
import cn.opensrcdevelop.ai.dto.TestDataSourceConnRequestDto;
import cn.opensrcdevelop.ai.dto.TestDataSourceConnResponseDto;
import cn.opensrcdevelop.ai.entity.DataSourceConf;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.enums.DataSourceType;
import cn.opensrcdevelop.ai.mapper.DataSourceConfMapper;
import cn.opensrcdevelop.ai.service.DataSourceConfService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.response.PageData;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.redisson.api.RLock;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@Slf4j
@Service
@RequiredArgsConstructor
public class DataSourceConfServiceImpl extends ServiceImpl<DataSourceConfMapper, DataSourceConf> implements DataSourceConfService {

    private static final String SYNC_TABLE_LOCK = "sync_table_lock:%s";

    private final TableService tableService;
    private final DataSourceMetaCollector dataSourceMetaCollector;

    /**
     * 获取已启用的数据源配置列表
     *
     * @return 已启用的数据源配置列表
     */
    @Override
    public List<DataSourceConfResponseDto> enabledList() {
        // 1. 查询数据库
        List<DataSourceConf> dataSourceConfList = super.list(Wrappers.<DataSourceConf>lambdaQuery()
                .select(DataSourceConf::getDataSourceId, DataSourceConf::getDataSourceName, DataSourceConf::getDescription)
                .eq(DataSourceConf::getEnabled, true)
                .orderByAsc(DataSourceConf::getDataSourceName));

        // 2. 属性编辑
        return CommonUtil.stream(dataSourceConfList).map(dataSourceConf -> DataSourceConfResponseDto.builder()
                .id(dataSourceConf.getDataSourceId())
                .name(dataSourceConf.getDataSourceName())
                .desc(dataSourceConf.getDescription())
                .build()).toList();
    }

    /**
     * 获取数据源配置列表
     *
     * @param keyword 数据源名称检索关键字
     * @param page    页数
     * @param size    条数
     * @return 数据源配置列表
     */
    @Override
    public PageData<DataSourceConfResponseDto> list(String keyword, int page, int size) {
        // 1. 查询数据库
        List<DataSourceConf> dataSourceConfList;
        Page<DataSourceConf> pageRequest = new Page<>(page, size);
        if (StringUtils.isNotEmpty(keyword)) {
            dataSourceConfList = super.list(pageRequest, Wrappers.<DataSourceConf>lambdaQuery()
                    .select(DataSourceConf::getDataSourceId,
                            DataSourceConf::getDataSourceName,
                            DataSourceConf::getDataSourceType,
                            DataSourceConf::getEnabled,
                            DataSourceConf::getLastSyncTableTime,
                            DataSourceConf::getSyncTableCount)
                    .like(DataSourceConf::getDataSourceName, keyword)
                    .orderByAsc(DataSourceConf::getDataSourceName)
            );
        } else {
            dataSourceConfList = super.list(pageRequest, Wrappers.<DataSourceConf>lambdaQuery()
                    .select(DataSourceConf::getDataSourceId,
                            DataSourceConf::getDataSourceName,
                            DataSourceConf::getDataSourceType,
                            DataSourceConf::getEnabled,
                            DataSourceConf::getLastSyncTableTime,
                            DataSourceConf::getSyncTableCount)
                    .orderByAsc(DataSourceConf::getDataSourceName)
            );
        }

        // 2. 属性编辑
        PageData<DataSourceConfResponseDto> pageData = new PageData<>();
        pageData.setPages(pageRequest.getPages());
        pageData.setCurrent(pageRequest.getCurrent());
        pageData.setTotal(pageRequest.getTotal());
        pageData.setSize(pageRequest.getSize());

        List<DataSourceConfResponseDto> data = CommonUtil.stream(dataSourceConfList).map(dataSourceConf -> DataSourceConfResponseDto.builder()
                        .id(dataSourceConf.getDataSourceId())
                        .name(dataSourceConf.getDataSourceName())
                        .type(DataSourceType.valueOf(dataSourceConf.getDataSourceType()).getName())
                        .enabled(dataSourceConf.getEnabled())
                        .lastSyncTableTime(dataSourceConf.getLastSyncTableTime())
                        .syncTableCount(dataSourceConf.getSyncTableCount())
                        .build())
                .toList();

        // 3. 统计表数量
        for (DataSourceConfResponseDto d : data) {
            if (Objects.nonNull(d.getLastSyncTableTime())) {
                d.setTableCount(tableService.count(Wrappers.<Table>lambdaQuery().eq(Table::getDataSourceId, d.getId())));
            }
        }
        pageData.setList(data);
        return pageData;
    }

    /**
     * 获取数据源配置详情
     *
     * @param id 数据源配置ID
     * @return 数据源配置详情
     */
    @Override
    public DataSourceConfResponseDto detail(String id) {
        // 1. 数据库操作
        DataSourceConf dataSourceConf = super.getById(id);
        if (Objects.isNull(dataSourceConf)) {
            return DataSourceConfResponseDto.builder().build();
        }

        // 2. 属性编辑
        return DataSourceConfResponseDto.builder()
                .id(dataSourceConf.getDataSourceId())
                .name(dataSourceConf.getDataSourceName())
                .type(dataSourceConf.getDataSourceType())
                .database(dataSourceConf.getDatabase())
                .schema(dataSourceConf.getSchema())
                .host(dataSourceConf.getHost())
                .port(dataSourceConf.getPort())
                .username(dataSourceConf.getUsername())
                .password(dataSourceConf.getPassword())
                .jdbcParams(dataSourceConf.getJdbcParams())
                .desc(dataSourceConf.getDescription())
                .build();
    }

    /**
     * 同步表
     *
     * @param id 数据源ID
     */
    @Override
    @SuppressWarnings({"java:S2222", "java:S3776"})
    public void syncTable(String id) {
        // 1. 检查数据源是否存在
        boolean exists = super
                .exists(Wrappers.<DataSourceConf>lambdaQuery()
                        .eq(DataSourceConf::getDataSourceId, id)
                        .eq(DataSourceConf::getEnabled, true));
        if (!exists) {
            throw new BizException(MessageConstants.AI_DATASOURCE_MSG_1000, id);
        }

        // 2. 检查是否正在同步
        if (RedisUtil.getLock(SYNC_TABLE_LOCK.formatted(id)).isLocked()) {
            throw new BizException(MessageConstants.AI_DATASOURCE_MSG_1001);
        }

        // 3. 采集数据源元数据
        CompletableFuture.runAsync(() -> {
                    // 3.1 获取锁
                    RLock lock = RedisUtil.getLock(SYNC_TABLE_LOCK.formatted(id));
                    try {
                        if (lock.tryLock()) {
                            // 3.2 采集
                            dataSourceMetaCollector.collect(id);
                            log.info("数据表同步成功，数据源ID：{}", id);
                        }
                    } finally {
                        if (lock.isLocked()) lock.unlock();
                    }
                }, SpringContextUtil.getBean(ExecutorConstants.EXECUTOR_IO_DENSE))
                .orTimeout(10, TimeUnit.MINUTES)
                .whenComplete((v, ex) -> {
                    if (Objects.nonNull(ex)) {
                        if (ex instanceof TimeoutException) {
                            log.error("数据表同步超时，数据源ID：{}", id, ex);
                        } else {
                            log.error("数据表同步失败，数据源ID：{}", id, ex);
                        }
                    }
                });
    }

    /**
     * 创建数据源配置
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void createDataSourceConf(DataSourceConfRequestDto requestDto) {
        // 1. 检查数据源名称是否存在
        checkDataSourceName(requestDto, null);

        // 2. 属性设置
        String dataSourceId = CommonUtil.getUUIDV7String();
        AuditContext.setSpelVariable("dataSourceId", dataSourceId);

        DataSourceConf dataSourceConf = new DataSourceConf();
        dataSourceConf.setDataSourceId(dataSourceId);
        dataSourceConf.setDataSourceName(requestDto.getName());
        dataSourceConf.setDataSourceType(requestDto.getType());
        dataSourceConf.setDescription(requestDto.getDesc());
        dataSourceConf.setDatabase(requestDto.getDatabase());
        dataSourceConf.setSchema(requestDto.getSchema());
        dataSourceConf.setHost(requestDto.getHost());
        dataSourceConf.setPort(requestDto.getPort());
        dataSourceConf.setUsername(requestDto.getUsername());
        dataSourceConf.setPassword(requestDto.getPassword());
        dataSourceConf.setJdbcParams(requestDto.getJdbcParams());
        dataSourceConf.setEnabled(true);

        // 3. 数据库操作
        super.save(dataSourceConf);
    }

    /**
     * 更新数据源配置
     *
     * @param requestDto 请求
     */
    @Transactional
    @Override
    public void updateDataSourceConf(DataSourceConfRequestDto requestDto) {
        String dataSourceId = requestDto.getId();

        // 1. 获取版本号
        var rawDataSourceConf = super.getById(dataSourceId);
        if (Objects.isNull(rawDataSourceConf)) {
            return;
        }

        // 2. 检查数据源名称是否存在
        checkDataSourceName(requestDto, rawDataSourceConf);

        // 3. 属性设置
        DataSourceConf updateDataSourceConf = new DataSourceConf();
        updateDataSourceConf.setDataSourceId(dataSourceId);
        updateDataSourceConf.setDataSourceName(requestDto.getName());
        updateDataSourceConf.setDescription(requestDto.getDesc());
        updateDataSourceConf.setHost(requestDto.getHost());
        updateDataSourceConf.setPort(requestDto.getPort());
        updateDataSourceConf.setUsername(requestDto.getUsername());
        updateDataSourceConf.setPassword(requestDto.getPassword());
        updateDataSourceConf.setJdbcParams(requestDto.getJdbcParams());
        CommonUtil.callSetWithCheck(Objects::nonNull, updateDataSourceConf::setEnabled, requestDto::getEnabled);

        // 4. 数据库操作
        super.updateById(updateDataSourceConf);
    }

    /**
     * 测试数据源连接
     *
     * @param requestDto 测试数据源连接请求
     * @return 测试数据源连接响应
     */
    @Override
    public TestDataSourceConnResponseDto testConn(TestDataSourceConnRequestDto requestDto) {
        DataSourceType dataSourceType = requestDto.getType();
        Connection connection = null;
        try {
            String jdbcUrl = dataSourceType.getJdbcUrl(
                    requestDto.getHost(),
                    requestDto.getPort(),
                    requestDto.getDatabase(),
                    ""
            );
            connection = DriverManager.getConnection(
                    jdbcUrl,
                    requestDto.getUsername(),
                    requestDto.getPassword()
            );
            return TestDataSourceConnResponseDto.builder().connected(true).build();
        } catch (SQLException e) {
            return TestDataSourceConnResponseDto.builder().connected(false).build();
        } finally {
            if (connection != null) {
                try {
                    connection.close();
                } catch (SQLException e) {
                    log.warn("关闭数据库连接时出错: {}", e.getMessage(), e);
                }
            }
        }
    }

    private void checkDataSourceName(DataSourceConfRequestDto requestDto, DataSourceConf rawDataSourceConf) {
        if (Objects.nonNull(rawDataSourceConf) && StringUtils.equals(requestDto.getName(), rawDataSourceConf.getDataSourceName())) {
            return;
        }

        if (Objects.nonNull(super.getOne(Wrappers.<DataSourceConf>lambdaQuery().eq(DataSourceConf::getDataSourceName, requestDto.getName())))) {
            throw new BizException(MessageConstants.AI_DATASOURCE_MSG_1002, requestDto.getName());
        }
    }
}
