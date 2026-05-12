import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { DynamicBorder } from "@mariozechner/pi-coding-agent";
import {
  Container,
  SelectList,
  Text,
  matchesKey,
  Key,
} from "@mariozechner/pi-tui";
import { readFile, writeFile, mkdir, access } from "node:fs/promises";
import path from "node:path";
import os from "node:os";

interface Permissions {
  paths?: Record<string, string[]>;
}

let stickyPermissions: Permissions = { paths: {} };
let sessionPermissions: Permissions = { paths: {} };
let permissionsFile: string | null = null;
let permissionsLoaded = false;

let globalWindowFocused = true;
let activeTui: any = null;

function onStdinData(chunk: Buffer) {
  const s = chunk.toString();
  if (s.includes("\x1b[I")) {
    globalWindowFocused = true;
    if (activeTui) activeTui.requestRender();
  } else if (s.includes("\x1b[O")) {
    globalWindowFocused = false;
    if (activeTui) activeTui.requestRender();
  }
}

async function getGitRootOrDir(startDir: string): Promise<string> {
  let dir = startDir;
  while (true) {
    try {
      await access(path.join(dir, ".git"));
      return dir;
    } catch (e) {
      const parent = path.dirname(dir);
      if (parent === dir || dir === os.homedir()) {
        break;
      }
      dir = parent;
    }
  }
  return startDir;
}

async function getPermissionsFile(): Promise<string> {
  if (permissionsFile) return permissionsFile;

  let dir = process.cwd();
  const rootDir = await getGitRootOrDir(dir);
  permissionsFile = path.join(rootDir, ".pi", "permissions.json");
  return permissionsFile;
}

async function loadPermissions(): Promise<void> {
  try {
    const file = await getPermissionsFile();
    const content = await readFile(file, "utf8");
    const parsed = JSON.parse(content);
    stickyPermissions = {
      paths: parsed.paths || {},
    };
  } catch (error: any) {
    if (error.code !== "ENOENT") {
      console.error(`Failed to load sticky permissions: ${error.message}`);
    }
  }
}

async function savePermissions(): Promise<void> {
  try {
    const file = await getPermissionsFile();
    await mkdir(path.dirname(file), { recursive: true });

    let diskPermissions: Permissions = { paths: {} };
    try {
      const content = await readFile(file, "utf8");
      diskPermissions = JSON.parse(content);
    } catch (e: any) {
      if (e.code !== "ENOENT") {
        console.error(
          `Failed to read sticky permissions for merge: ${e.message}`,
        );
      }
    }

    if (!diskPermissions.paths) diskPermissions.paths = {};
    for (const [prefix, tools] of Object.entries(
      stickyPermissions.paths || {},
    )) {
      if (!diskPermissions.paths[prefix]) {
        diskPermissions.paths[prefix] = [];
      }
      for (const tool of tools) {
        if (!diskPermissions.paths[prefix].includes(tool)) {
          diskPermissions.paths[prefix].push(tool);
        }
      }
    }

    stickyPermissions = diskPermissions;

    await writeFile(file, JSON.stringify(stickyPermissions, null, 2), "utf8");
  } catch (error: any) {
    console.error(`Failed to save sticky permissions: ${error.message}`);
  }
}

function getRequiredPermissions(
  toolName: string,
  input: any,
): { tool: string; paths: string[] } | null {
  if (toolName === "bash" && input.command) {
    return { tool: "bash", paths: [process.cwd()] };
  } else if (["write", "read", "edit"].includes(toolName) && input.path) {
    return { tool: toolName, paths: [input.path] };
  } else if (["grep", "find", "ls"].includes(toolName)) {
    return { tool: toolName, paths: [input.path || process.cwd()] };
  }
  return null;
}

function isPiReadAllowed(
  toolName: string,
  input: any,
  req: { paths: string[] } | null,
): boolean {
  if (["read", "grep", "find", "ls"].includes(toolName)) {
    if (!req) return false;
    const isPiPath = (p: string) =>
      path.resolve(process.cwd(), p).includes("pi-coding-agent/");
    return req.paths.every(isPiPath);
  }
  return false;
}

function isAllowedByPermissions(
  permissions: Permissions,
  toolName: string,
  input: any,
): boolean {
  const req = getRequiredPermissions(toolName, input);

  if (isPiReadAllowed(toolName, input, req)) return true;

  if (!req || toolName === "bash") return false;

  for (const p of req.paths) {
    const checkPath = path.isAbsolute(p) ? p : path.resolve(process.cwd(), p);
    let pathAllowed = false;
    for (const [prefix, tools] of Object.entries(permissions.paths || {})) {
      const normalizedPrefix = prefix.endsWith(path.sep)
        ? prefix
        : prefix + path.sep;
      const normalizedPath = checkPath.endsWith(path.sep)
        ? checkPath
        : checkPath + path.sep;
      if (
        normalizedPath.startsWith(normalizedPrefix) &&
        tools.includes(req.tool)
      ) {
        pathAllowed = true;
        break;
      }
    }
    if (!pathAllowed) return false;
  }
  return true;
}

function isStickyAllowed(toolName: string, input: any): boolean {
  return isAllowedByPermissions(stickyPermissions, toolName, input);
}

function isSessionAllowed(toolName: string, input: any): boolean {
  return isAllowedByPermissions(sessionPermissions, toolName, input);
}

function addPermission(
  permissions: Permissions,
  prefix: string,
  toolName: string,
  input: any,
): void {
  const req = getRequiredPermissions(toolName, input);
  if (!req) return;

  if (!permissions.paths) permissions.paths = {};
  if (!permissions.paths[prefix]) {
    permissions.paths[prefix] = [];
  }
  if (!permissions.paths[prefix].includes(req.tool)) {
    permissions.paths[prefix].push(req.tool);
  }
}

function addStickyPermission(
  prefix: string,
  toolName: string,
  input: any,
): void {
  addPermission(stickyPermissions, prefix, toolName, input);
}

function addSessionPermission(
  prefix: string,
  toolName: string,
  input: any,
): void {
  addPermission(sessionPermissions, prefix, toolName, input);
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", () => {
    process.stdin.on("data", onStdinData);
    process.stdout.write("\x1b[?1004h");
  });

  pi.on("session_shutdown", () => {
    process.stdin.off("data", onStdinData);
    process.stdout.write("\x1b[?1004l");
  });

  pi.on("tool_call", async (event, ctx) => {
    if (!permissionsLoaded) {
      await loadPermissions();
      permissionsLoaded = true;
    }

    // We intercept all built-in filesystem and shell tools
    const restrictedTools = [
      "bash",
      "write",
      "edit",
      "read",
      "grep",
      "find",
      "ls",
    ];

    if (restrictedTools.includes(event.toolName)) {
      if (event.toolName === "bash" && event.input.command) {
        const dangerousPatterns = [
          /\brm\s+(?:[^;|&]*?\s)?(?:-[a-zA-Z]*[rR][a-zA-Z]*|--recursive)/i,
          /\bsudo\b/i,
          /\b(chmod|chown)\b.*777/i,
        ];
        if (
          dangerousPatterns.some((p) => p.test(event.input.command as string))
        ) {
          return {
            block: true,
            reason: "Dangerous command explicitly blocked",
          };
        }
      }

      const req = getRequiredPermissions(event.toolName, event.input);

      if (req && isStickyAllowed(event.toolName, event.input)) {
        return;
      }

      if (req && isSessionAllowed(event.toolName, event.input)) {
        return;
      }

      if (!ctx.hasUI) {
        return {
          block: true,
          reason: "Tool call blocked: No UI available for confirmation",
        };
      }

      let confirmationMessage = `Allow '${event.toolName}' with arguments: ${JSON.stringify(event.input, null, 2)}?`;

      if (event.toolName === "bash") {
        confirmationMessage = `Run: ${event.input.command}?`;
      } else if (event.toolName === "write") {
        confirmationMessage = `Write: ${event.input.path}?`;
      } else if (event.toolName === "read") {
        confirmationMessage = `Read: ${event.input.path}?`;
      } else if (event.toolName === "edit") {
        confirmationMessage = `Edit: ${event.input.path}?`;
      } else if (["grep", "find", "ls"].includes(event.toolName)) {
        confirmationMessage = `${event.toolName}: ${event.input.path || process.cwd()}?`;
      }

      let alwaysLabel = "Always (dir)";
      let rootDir = process.cwd();
      const isBash = event.toolName === "bash";

      if (req && !isBash) {
        const workspaceRoot = await getGitRootOrDir(process.cwd());
        let targetPath = event.input.path || process.cwd();
        if (!path.isAbsolute(targetPath)) {
          targetPath = path.resolve(process.cwd(), targetPath);
        }

        if (targetPath.startsWith(workspaceRoot)) {
          rootDir = workspaceRoot;
          alwaysLabel = `Always (${path.basename(rootDir)})`;
        } else {
          rootDir = path.dirname(targetPath);
          alwaysLabel = `Always (${rootDir})`;
        }
      }

      let sessionLabel = "Session (dir)";
      if (req && !isBash) {
        sessionLabel = `Session (${rootDir})`;
      }

      const yesOpt = { value: "", label: "" };
      const optionsItems = [yesOpt, { value: "No", label: "No" }];
      let tabState = 0; // 0:Yes, 1:Session, 2:Always

      function updateTabLabel() {
        if (!req || isBash) {
          yesOpt.value = "Yes";
          yesOpt.label = "Yes";
          return;
        }
        if (tabState === 0) {
          yesOpt.value = "Yes";
          yesOpt.label = "Yes [Tab for Session]";
        } else if (tabState === 1) {
          yesOpt.value = "Session";
          yesOpt.label = `${sessionLabel} [Tab for Always]`;
        } else {
          yesOpt.value = "Always";
          yesOpt.label = `${alwaysLabel} [Tab for Yes]`;
        }
      }
      updateTabLabel();

      let choice: string | null = null;
      while (true) {
        choice = await ctx.ui.custom<string | null>((tui, theme, kb, done) => {
          activeTui = tui;
          const container = new Container();
          container.addChild(
            new DynamicBorder((s: string) =>
              globalWindowFocused ? theme.fg("accent", s) : theme.fg("dim", s),
            ),
          );

          container.addChild(
            new Text(
              theme.fg("accent", theme.bold("Permission Required")),
              1,
              0,
            ),
          );
          container.addChild(
            new Text(theme.fg("text", confirmationMessage), 1, 1),
          );

          const selectList = new SelectList(optionsItems, optionsItems.length, {
            selectedPrefix: (t) => theme.fg("accent", t),
            selectedText: (t) => theme.fg("accent", t),
            description: (t) => theme.fg("muted", t),
          });

          selectList.onSelect = (item) => done(item.value);
          selectList.onCancel = () => done(null);
          container.addChild(selectList);

          // Force vertical padding by wrapping the empty text in a layout container if needed,
          // or just embedding the newline into the hint string.
          container.addChild(
            new Text(
              theme.fg("dim", "\n jk/↑↓ nav • ↵ select • esc cancel"),
              1,
              0,
            ),
          );

          container.addChild(
            new DynamicBorder((s: string) =>
              globalWindowFocused ? theme.fg("accent", s) : theme.fg("dim", s),
            ),
          );

          const onAbort = () => done(null);
          if (ctx.signal?.aborted) done(null);
          ctx.signal?.addEventListener("abort", onAbort);

          return {
            render: (w) => {
              const lines = container.render(w);
              if (!globalWindowFocused) {
                return lines.map((line) =>
                  theme.fg("dim", line.replace(/\x1b\[[0-9;]*m/g, "")),
                );
              }
              return lines;
            },
            invalidate: () => container.invalidate(),
            handleInput: (data: string) => {
              if (data === "\x1b[I" || data === "\x1b[O") return;

              if (kb.matches(data, "app.tools.expand")) {
                ctx.ui.setToolsExpanded(!ctx.ui.getToolsExpanded());
                return;
              }

              if (
                data === "\t" &&
                req &&
                !isBash &&
                selectList.selectedIndex === 0
              ) {
                tabState = (tabState + 1) % 3;
                updateTabLabel();
                tui.requestRender();
                return;
              }

              const isNav =
                data === "j" ||
                data === "k" ||
                matchesKey(data, Key.up) ||
                matchesKey(data, Key.down) ||
                matchesKey(data, Key.enter) ||
                matchesKey(data, Key.escape);

              if (isNav) {
                if (data === "j")
                  selectList.handleInput("\x1b[B"); // Key.down
                else if (data === "k")
                  selectList.handleInput("\x1b[A"); // Key.up
                else selectList.handleInput(data);
                tui.requestRender();
              }
            },
            dispose: () => {
              activeTui = null;
              ctx.signal?.removeEventListener("abort", onAbort);
            },
          };
        });

        if (!choice || choice === "No") {
          ctx.abort();
          return { block: true, reason: "Blocked by user" };
        } else if (choice === "Session") {
          if (req) {
            let prefix = rootDir;
            if (!prefix.endsWith(path.sep)) prefix += path.sep;

            addSessionPermission(prefix, event.toolName, event.input);
            ctx.ui.notify(
              `Added session permission for '${req.tool}' (${prefix}).`,
              "info",
            );
          }
          break;
        } else if (choice === "Always") {
          if (req) {
            let prefix = rootDir;
            if (!prefix.endsWith(path.sep)) prefix += path.sep;

            addStickyPermission(prefix, event.toolName, event.input);
            await savePermissions();
            ctx.ui.notify(
              `Added permission for '${req.tool}' (${prefix}).`,
              "info",
            );
          }
          break;
        } else {
          break;
        }
      }
    }
  });
}
