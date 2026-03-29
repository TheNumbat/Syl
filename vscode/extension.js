const vscode = require("vscode");
const { execSync } = require("child_process");

/** @type {vscode.DiagnosticCollection} */
let diagnostics;

/** @type {Map<string, NodeJS.Timeout>} */
const debounceTimers = new Map();

const DEBOUNCE_MS = 500;

function activate(context) {
    diagnostics = vscode.languages.createDiagnosticCollection("syl");
    context.subscriptions.push(diagnostics);

    const formatter = vscode.languages.registerDocumentFormattingEditProvider("syl", {
        provideDocumentFormattingEdits(document) {
            const config = vscode.workspace.getConfiguration("syl");
            const command = config.get("formatCommand", "dune exec bin/syl.exe -- fmt");
            const cwd = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

            try {
                const formatted = execSync(command, {
                    cwd,
                    encoding: "utf-8",
                    input: document.getText(),
                    timeout: 10000,
                });
                diagnostics.delete(document.uri);
                const fullRange = new vscode.Range(
                    document.lineAt(0).range.start,
                    document.lineAt(document.lineCount - 1).range.end,
                );
                return [vscode.TextEdit.replace(fullRange, formatted)];
            } catch (err) {
                setDiagnostics(document, err.stderr || "");
                return [];
            }
        },
    });

    const onDidChange = vscode.workspace.onDidChangeTextDocument((event) => {
        if (event.document.languageId !== "syl") return;
        scheduleCheck(event.document);
    });

    const onDidOpen = vscode.workspace.onDidOpenTextDocument((document) => {
        if (document.languageId !== "syl") return;
        checkDocument(document);
    });

    const onDidClose = vscode.workspace.onDidCloseTextDocument((document) => {
        diagnostics.delete(document.uri);
    });

    context.subscriptions.push(formatter, onDidChange, onDidOpen, onDidClose);
}

function scheduleCheck(document) {
    const key = document.uri.toString();
    const existing = debounceTimers.get(key);
    if (existing) clearTimeout(existing);
    debounceTimers.set(
        key,
        setTimeout(() => {
            debounceTimers.delete(key);
            checkDocument(document);
        }, DEBOUNCE_MS),
    );
}

function checkDocument(document) {
    const config = vscode.workspace.getConfiguration("syl");
    const command = config.get("checkCommand", "dune exec bin/syl.exe -- check");
    const cwd = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;

    try {
        execSync(command, {
            cwd,
            encoding: "utf-8",
            input: document.getText(),
            timeout: 10000,
        });
        diagnostics.delete(document.uri);
    } catch (err) {
        setDiagnostics(document, err.stderr || "");
    }
}

function setDiagnostics(document, stderr) {
    try {
        const parsed = JSON.parse(stderr.trim());
        const line = Math.max(0, parsed.line - 1);
        const col = Math.max(0, parsed.column);
        const endChar = document.lineAt(line).range.end.character;
        const diag = new vscode.Diagnostic(
            new vscode.Range(line, col, line, endChar),
            parsed.reason,
            vscode.DiagnosticSeverity.Error,
        );
        diag.source = "syl";
        diagnostics.set(document.uri, [diag]);
    } catch {
        diagnostics.delete(document.uri);
    }
}

function deactivate() {
    for (const timer of debounceTimers.values()) clearTimeout(timer);
    debounceTimers.clear();
}

module.exports = { activate, deactivate };
