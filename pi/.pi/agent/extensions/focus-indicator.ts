import { CustomEditor, type ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.on("session_start", (_event, ctx) => {
    const globalTheme = ctx.ui.theme;

    process.stdout.write("\x1b[?1004h");

    class FocusAwareEditor extends CustomEditor {
      private windowFocused = true;

      handleInput(data: string): void {
        if (data === "\x1b[I") {
          this.windowFocused = true;
          if (this.tui) this.tui.requestRender();
          return;
        }

        if (data === "\x1b[O") {
          this.windowFocused = false;
          if (this.tui) this.tui.requestRender();
          return;
        }

        super.handleInput(data);
      }

      override render(width: number): string[] {
        this.borderColor = this.windowFocused
          ? (s: string) => globalTheme.fg("accent", s)
          : (s: string) => globalTheme.fg("dim", s);

        const promptText = this.windowFocused ? " ➜ " : " ⏸ ";
        const padText = "   ";
        const promptWidth = padText.length;

        const lines = super.render(width - promptWidth);
        if (lines.length === 0) return lines;

        const styledPrompt = this.windowFocused
          ? globalTheme.fg("accent", globalTheme.bold(promptText))
          : globalTheme.fg("dim", globalTheme.bold(promptText));

        const borderExtension = this.borderColor("───");

        return lines.map((line, i) => {
          if (i === 0 || i === lines.length - 1) {
            return line + borderExtension;
          }

          const content = this.windowFocused
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
}
