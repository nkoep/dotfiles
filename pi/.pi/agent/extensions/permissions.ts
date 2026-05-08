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
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);

interface Permissions {
  paths?: Record<string, string[]>;
}

let stickyPermissions: Permissions = { paths: {} };
let permissionsFile: string | null = null;
let permissionsLoaded = false;

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
    const cmd: string = input.command.trim();
    if (/[><;&$`|]/.test(cmd)) return null;
    const parts = cmd.split(/\s+/);
    const baseCmd = parts[0];
    const pathArgs = parts
      .slice(1)
      .filter((arg) => !arg.startsWith("-") && arg.length > 0);
    const paths = pathArgs.length > 0 ? pathArgs : [process.cwd()];
    return { tool: `bash(${baseCmd})`, paths };
  } else if (["write", "read", "edit"].includes(toolName) && input.path) {
    return { tool: toolName, paths: [input.path] };
  }
  return null;
}

function isStickyAllowed(toolName: string, input: any): boolean {
  const req = getRequiredPermissions(toolName, input);
  if (!req) return false;

  for (const p of req.paths) {
    const checkPath = path.isAbsolute(p) ? p : path.resolve(process.cwd(), p);
    let pathAllowed = false;
    for (const [prefix, tools] of Object.entries(
      stickyPermissions.paths || {},
    )) {
      if (checkPath.startsWith(prefix) && tools.includes(req.tool)) {
        pathAllowed = true;
        break;
      }
    }
    if (!pathAllowed) return false;
  }
  return true;
}

function addStickyPermission(
  prefix: string,
  toolName: string,
  input: any,
): void {
  const req = getRequiredPermissions(toolName, input);
  if (!req) return;

  if (!stickyPermissions.paths) stickyPermissions.paths = {};
  if (!stickyPermissions.paths[prefix]) {
    stickyPermissions.paths[prefix] = [];
  }
  if (!stickyPermissions.paths[prefix].includes(req.tool)) {
    stickyPermissions.paths[prefix].push(req.tool);
  }
}

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", async (event, ctx) => {
    if (!permissionsLoaded) {
      await loadPermissions();
      permissionsLoaded = true;
    }

    const dangerousTools = ["bash", "write", "edit", "read"];

    if (dangerousTools.includes(event.toolName)) {
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

      if (!ctx.hasUI) {
        return {
          block: true,
          reason: "Tool call blocked: No UI available for confirmation",
        };
      }

      let confirmationMessage = `Allow tool '${event.toolName}' with arguments: ${JSON.stringify(event.input, null, 2)}?`;

      if (event.toolName === "bash") {
        confirmationMessage = `Allow bash command: ${event.input.command}?`;
      } else if (event.toolName === "write") {
        confirmationMessage = `Allow writing to file: ${event.input.path}?`;
      } else if (event.toolName === "read") {
        confirmationMessage = `Allow reading from file: ${event.input.path}?`;
      } else if (event.toolName === "edit") {
        confirmationMessage = `Allow editing file: ${event.input.path}?`;
      }

      let alwaysLabel = "Always (dir)";
      let rootDir = process.cwd();

      if (req) {
        const workspaceRoot = await getGitRootOrDir(process.cwd());
        let targetPath = process.cwd();

        if (event.toolName !== "bash" && event.input.path) {
          targetPath = event.input.path;
          if (!path.isAbsolute(targetPath)) {
            targetPath = path.resolve(process.cwd(), targetPath);
          }
        }

        if (targetPath.startsWith(workspaceRoot)) {
          rootDir = workspaceRoot;
          try {
            await access(path.join(rootDir, ".git"));
            alwaysLabel = "Always (repo)";
          } catch {
            alwaysLabel = "Always (dir)";
          }
        } else {
          rootDir =
            event.toolName === "bash"
              ? process.cwd()
              : path.dirname(targetPath);
          alwaysLabel = "Always (dir)";
        }
      }

      const optionsItems = [{ value: "Yes", label: "Yes" }];
      if (req) {
        optionsItems.push({ value: "Always", label: alwaysLabel });
      }

      let choice: string | null = null;
      while (true) {
        choice = await ctx.ui.custom<string | null>((tui, theme, kb, done) => {
          const container = new Container();
          container.addChild(
            new DynamicBorder((s: string) => theme.fg("accent", s)),
          );

          container.addChild(
            new Text(theme.fg("accent", theme.bold("Tool Confirmation")), 1, 0),
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

          container.addChild(
            new DynamicBorder((s: string) => theme.fg("accent", s)),
          );

          const onAbort = () => done(null);
          if (ctx.signal?.aborted) done(null);
          ctx.signal?.addEventListener("abort", onAbort);

          return {
            render: (w) => container.render(w),
            invalidate: () => container.invalidate(),
            handleInput: (data: string) => {
              if (kb.matches(data, "app.tools.expand")) {
                ctx.ui.setToolsExpanded(!ctx.ui.getToolsExpanded());
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
            dispose: () => ctx.signal?.removeEventListener("abort", onAbort),
          };
        });

        if (!choice) {
          ctx.abort();
          return { block: true, reason: "Blocked by user" };
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
