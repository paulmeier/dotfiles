;;; $DOOMDIR/cli.el -*- lexical-binding: t; -*-

(with-eval-after-load 'doom-cli-loaddefs
  (setq doom-env-deny
        (append '("^CLAUDE" "^ANTHROPIC_" "^AI_AGENT$" "^MCP_" "^BAGGAGE$"
                  "^API_TIMEOUT_MS$" "^DISABLE_\\(AUTOUPDATER\\|MICROCOMPACT\\)$"
                  "^USE_\\(LOCAL\\|STAGING\\)_OAUTH$" "^GIT_EDITOR$"
                  "^COREPACK_ENABLE_AUTO_PIN$" "^NODE_USE_SYSTEM_CA$"
                  "^NoDefaultCurrentDirectoryInExePath$" "^OSLogRateLimit$")
                doom-env-deny))
  (add-to-list 'doom-env-allow "^CLAUDE_CODE_OAUTH_TOKEN$"))
