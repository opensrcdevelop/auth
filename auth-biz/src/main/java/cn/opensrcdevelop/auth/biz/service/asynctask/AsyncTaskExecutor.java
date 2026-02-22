package cn.opensrcdevelop.auth.biz.service.asynctask;

/**
 * 异步任务执行器接口 业务模块实现此接口来处理具体的异步任务
 */
public interface AsyncTaskExecutor {

    /**
     * 获取任务类型
     *
     * @return 任务类型
     */
    String getTaskType();

    /**
     * 执行任务
     *
     * @param taskId
     *            任务ID
     * @param taskParams
     *            任务参数（JSON格式）
     * @param context
     *            执行上下文，包含进度更新回调、结果存储等
     */
    void execute(String taskId, String taskParams, TaskExecutionContext context);

    /**
     * 任务执行上下文
     */
    interface TaskExecutionContext {

        /**
         * 更新任务进度
         *
         * @param progress
         *            进度（0-100）
         */
        void updateProgress(int progress);

        /**
         * 设置任务结果
         *
         * @param result
         *            任务结果（JSON格式）
         */
        void setResult(String result);

        /**
         * 存储结果文件
         *
         * @param data
         *            文件数据
         * @param fileName
         *            文件名
         * @return 文件存储路径
         */
        String storeResultFile(byte[] data, String fileName);
    }
}
