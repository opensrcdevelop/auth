import * as monaco from "monaco-editor";
import { nextTick, onBeforeUnmount, ref } from "vue";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import JsonWorker from "monaco-editor/esm/vs/language/json/json.worker?worker";

export const useMonacoEditor = (language: string) => {
  let monocaEditor: monaco.editor.IStandaloneCodeEditor | null = null;
  const monacoEditorRef = ref<HTMLElement | null>(null);

  function createEditor(
    editorOption: monaco.editor.IStandaloneEditorConstructionOptions = {}
  ) {
    if (!monacoEditorRef.value) return;

    self.MonacoEnvironment = {
      getWorker(workerId, label) {
        if (label === "json") {
          return new JsonWorker();
        }
        return new EditorWorker();
      },
    };
    
    monocaEditor = monaco.editor.create(monacoEditorRef.value, {
      model: monaco.editor.createModel("", language),
      minimap: { enabled: false },
      roundedSelection: true,
      theme: "vs",
      multiCursorModifier: "ctrlCmd",
      scrollbar: {
        verticalScrollbarSize: 10,
        horizontalScrollbarSize: 10,
      },
      lineNumbers: "on",
      tabSize: 2,
      fontSize: 14,
      automaticLayout: true,
      ...editorOption,
    });

    return monocaEditor;
  }

  async function formatDoc() {
    await monocaEditor?.getAction("editor.action.formatDocument")?.run();
  }

  function updateVal(val: string) {
    nextTick(() => {
      monocaEditor?.setValue(val);
      setTimeout(async () => {
        await formatDoc();
      }, 10);
    });
  }

  function getOption(name: monaco.editor.EditorOption) {
    return monocaEditor?.getOption(name);
  }

  function getEditor() {
    return monocaEditor;
  }

  function updateOptions(
    opt: monaco.editor.IStandaloneEditorConstructionOptions
  ) {
    monocaEditor?.updateOptions(opt);
  }

  onBeforeUnmount(() => {
    if (monocaEditor) {
      monocaEditor.dispose();
    }
  });

  return {
    monacoEditorRef,
    createEditor,
    getEditor,
    updateVal,
    updateOptions,
    getOption,
    formatDoc,
  };
};
