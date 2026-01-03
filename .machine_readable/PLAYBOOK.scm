;; SPDX-License-Identifier: AGPL-3.0-or-later
;; PLAYBOOK.scm - Operational runbook for modshells

(define playbook
  `((version . "1.0.0")
    (procedures
      ((deploy . (("build" . "just build")
                  ("test" . "just test")
                  ("release" . "just release")))
       (rollback . ())
       (debug . ())))
    (alerts . ())
    (contacts . ())))
