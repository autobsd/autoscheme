(foreign-declaration (include-string "declarations.h"))
(foreign-definition (include-string "definitions.c"))
(foreign-initialization (include-string "initialization.c"))



(define-library (auto scheme process context)
  (import (only (s7) exit emergency-exit))
  (export exit emergency-exit)

  (export command-line current-directory)
  )

(let ()
  (import (only (s7) rootlet))
  (environment-remove! (rootlet) 'command-line 'current-directory))
