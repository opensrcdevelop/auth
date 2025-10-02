class MarkdownHandler {
  private static instance: MarkdownHandler
  public isBound = false
  public handler: ((event: MouseEvent) => void) | null = null

  private constructor() {}

  public static getInstance(): MarkdownHandler {
    if (!MarkdownHandler.instance) {
      MarkdownHandler.instance = new MarkdownHandler()
    }
    return MarkdownHandler.instance
  }
}

export default MarkdownHandler;