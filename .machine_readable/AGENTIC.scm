;; SPDX-License-Identifier: AGPL-3.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for modshells

(define agentic-config
  `((version . "1.0.0")
    (claude-code
      ((model . "claude-opus-4-5-20251101")
       (tools . ("read" "edit" "bash" "grep" "glob"))
       (permissions . "read-all")))
    (patterns
      ((code-review . "thorough")
       (refactoring . "conservative")
       (testing . "comprehensive")))
    (constraints
      ((languages . ())
       (banned . ("typescript" "go" "python" "makefile"))))))
