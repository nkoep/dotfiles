import { VERSION } from "@mariozechner/pi-coding-agent";
import * as path from "path";
import { visibleWidth, truncateToWidth } from "@mariozechner/pi-tui";

function formatTokens(count) {
  if (count < 1000) return count.toString();
  if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
  if (count < 1000000) return `${Math.round(count / 1000)}k`;
  if (count < 10000000) return `${(count / 1000000).toFixed(1)}M`;
  return `${Math.round(count / 1000000)}M`;
}

export default function (pi) {
  let latestVersion = null;

  pi.on("session_start", async (_event, ctx) => {
    let tuiRef = null;

    fetch("https://pi.dev/api/latest-version")
      .then((res) => res.json())
      .then((data) => {
        if (data && data.version && data.version !== VERSION) {
          latestVersion = data.version;
          if (tuiRef) tuiRef.requestRender();
        }
      })
      .catch(() => {});

    ctx.ui.setFooter((tui, theme, footerData) => {
      tuiRef = tui;
      const unsub = footerData.onBranchChange(() => tui.requestRender());

      return {
        dispose: unsub,
        invalidate() {},
        render(width) {
          try {
            let cwd = ctx.cwd;
            const home = process.env.HOME || process.env.USERPROFILE;
            if (home && cwd.startsWith(home)) {
              cwd = `~${cwd.slice(home.length)}`;
            }
            let line1Left = theme.fg("accent", cwd);
            let line1Right = "";
            const branch = footerData.getGitBranch();
            if (branch) {
              const repoName = path.basename(ctx.cwd);
              line1Right =
                theme.fg("dim", repoName) +
                " " +
                theme.fg("warning", `(${branch})`);
            }

            let line1Center = "π";

            let line1LeftWidth = visibleWidth(line1Left);
            let line1CenterWidth = visibleWidth(line1Center);
            let line1RightWidth = visibleWidth(line1Right);

            let idealCenterStart1 =
              Math.floor(width / 2) - Math.floor(line1CenterWidth / 2);
            let padLeft1 = Math.max(1, idealCenterStart1 - line1LeftWidth);
            let actualCenterEnd1 = line1LeftWidth + padLeft1 + line1CenterWidth;
            let padRight1 = Math.max(
              1,
              width - actualCenterEnd1 - line1RightWidth,
            );

            let line1 =
              line1Left +
              " ".repeat(padLeft1) +
              line1Center +
              " ".repeat(padRight1) +
              line1Right;
            line1 = truncateToWidth(line1, width);

            let input = 0,
              output = 0,
              cacheRead = 0;
            for (const e of ctx.sessionManager.getBranch()) {
              if (e.type === "message" && e.message.role === "assistant") {
                input += e.message.usage?.input || 0;
                output += e.message.usage?.output || 0;
                cacheRead += e.message.usage?.cacheRead || 0;
              }
            }

            const contextUsage = ctx.getContextUsage();
            const contextTokens = contextUsage?.tokens || 0;
            const contextWindow =
              contextUsage?.contextWindow || ctx.model?.contextWindow || 0;
            const contextPercentValue = contextUsage?.percent || 0;
            const contextPercentStr =
              contextUsage?.percent != null
                ? contextPercentValue.toFixed(1)
                : "?";

            const contextColor =
              contextPercentValue > 90
                ? "error"
                : contextPercentValue > 70
                  ? "warning"
                  : "muted";

            const cacheColor =
              contextPercentValue > 90
                ? "warning"
                : contextPercentValue > 50
                  ? "accent"
                  : "dim";

            let statsStr =
              theme.fg("accent", "↑") +
              theme.fg("muted", formatTokens(input)) +
              " " +
              theme.fg("success", "↓") +
              theme.fg("muted", formatTokens(output)) +
              " " +
              theme.fg("border", "|") +
              " " +
              theme.fg(cacheColor, formatTokens(cacheRead)) +
              theme.fg("dim", " cached") +
              " " +
              theme.fg("border", "|") +
              " " +
              theme.fg(
                contextColor,
                `${formatTokens(contextTokens)} / ${formatTokens(contextWindow)}`,
              ) +
              theme.fg("dim", " (") +
              theme.fg(contextColor, `${contextPercentStr} %`) +
              theme.fg("dim", ")");

            const provider = ctx.model?.provider || "unknown";
            const modelId = ctx.model?.id || "unknown";

            let centerStr = `${theme.fg("text", modelId)} ${theme.fg("dim", `(${provider})`)}`;

            let line2Left = statsStr;

            let versionStr = theme.fg("dim", `v${VERSION}`);
            if (latestVersion) {
              versionStr += theme.fg(
                "success",
                ` (v${latestVersion} available)`,
              );
            }

            let line2Right = versionStr;

            let line2LeftVisible = visibleWidth(line2Left);
            let centerVisible = visibleWidth(centerStr);
            let line2RightVisible = visibleWidth(line2Right);

            let idealCenterStart =
              Math.floor(width / 2) - Math.floor(centerVisible / 2);
            let padLeft = Math.max(1, idealCenterStart - line2LeftVisible);
            let actualCenterEnd = line2LeftVisible + padLeft + centerVisible;
            let padRight = Math.max(
              1,
              width - actualCenterEnd - line2RightVisible,
            );

            let line2 =
              line2Left +
              " ".repeat(padLeft) +
              centerStr +
              " ".repeat(padRight) +
              line2Right;
            line2 = truncateToWidth(line2, width);

            return [line1, line2];
          } catch (err) {
            const errorMsg = `Footer Error: ${err.message}`.replace(
              /[\r\n]/g,
              " ",
            );
            return [truncateToWidth(errorMsg, width), ""];
          }
        },
      };
    });
  });
}
