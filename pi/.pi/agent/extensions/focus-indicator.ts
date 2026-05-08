import { CustomEditor, type ExtensionAPI } from "@mariozechner/pi-coding-agent";

let globalWindowFocused = true;
let activeEditorTui: any = null;

function onStdinData(chunk: Buffer) {
  const s = chunk.toString();
  if (s.includes("\x1b[I")) {
    globalWindowFocused = true;
    if (activeEditorTui) activeEditorTui.requestRender();
  } else if (s.includes("\x1b[O")) {
    globalWindowFocused = false;
    if (activeEditorTui) activeEditorTui.requestRender();
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    const globalTheme = ctx.ui.theme;

    // Track focus globally so events aren't swallowed by overlay components
    // (like tool prompts)
    process.stdin.on("data", onStdinData);

    // Enable Focus Tracking
    process.stdout.write("\x1b[?1004h");

    class FocusAwareEditor extends CustomEditor {
      constructor(tui: any, theme: any, keybindings: any) {
        super(tui, theme, keybindings);
        activeEditorTui = tui;
      }

      handleInput(data: string): void {
        // Prevent passing focus sequences to the underlying editor
        if (data === "\x1b[I" || data === "\x1b[O") {
          return;
        }

        super.handleInput(data);
      }

      override render(width: number): string[] {
        this.borderColor = globalWindowFocused
          ? (s: string) => globalTheme.fg("accent", s)
          : (s: string) => globalTheme.fg("dim", s);

        const promptText = globalWindowFocused ? " ➜ " : " ⏸ ";
        const padText = "   ";
        const promptWidth = padText.length;

        const lines = super.render(width - promptWidth);
        if (lines.length === 0) return lines;

        const styledPrompt = globalWindowFocused
          ? globalTheme.fg("accent", globalTheme.bold(promptText))
          : globalTheme.fg("dim", globalTheme.bold(promptText));

        const borderExtension = this.borderColor("───");

        return lines.map((line, i) => {
          if (i === 0 || i === lines.length - 1) {
            return line + borderExtension;
          }

          const content = globalWindowFocused
            ? line
            : globalTheme.fg("dim", line);

          if (i === 1) {
            return styledPrompt + content;
          }
          return padText + content;
        });
      }
    }

    ctx.ui.setEditorComponent((tui, theme, keybindings) => {
      return new FocusAwareEditor(tui, theme, keybindings);
    });
  });

  pi.on("session_shutdown", () => {
    process.stdin.off("data", onStdinData);
    // Disable Focus Tracking on shutdown
    process.stdout.write("\x1b[?1004l");
  });
}
