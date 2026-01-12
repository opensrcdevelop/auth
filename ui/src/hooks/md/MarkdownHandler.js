var MarkdownHandler = /** @class */ (function () {
    function MarkdownHandler() {
        this.isBound = false;
        this.handler = null;
    }
    MarkdownHandler.getInstance = function () {
        if (!MarkdownHandler.instance) {
            MarkdownHandler.instance = new MarkdownHandler();
        }
        return MarkdownHandler.instance;
    };
    return MarkdownHandler;
}());
export default MarkdownHandler;
