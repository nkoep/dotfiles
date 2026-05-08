import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerCommand("system", {
    description: "Show the fully assembled system prompt",
    handler: async (_args, ctx) => {
      const prompt = ctx.getSystemPrompt();
      ctx.ui.notify(prompt, "info");
    },
  });
}
