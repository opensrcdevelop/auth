package cn.opensrcdevelop.common.response;

import lombok.Data;

import java.util.List;

/**
 * 分页数据
 */
@Data
public class PageData<T> {

    /** 页数 */
    private Long pages;

    /** 分页大小 */
    private Long size;

    /** 总数 */
    private Long total;

    /** 当前页 */
    private Long current;

    /** 分页数据 */
    private List<T> list;
}
