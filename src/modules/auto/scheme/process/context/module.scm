(foreign-declaration (include-string "declarations.h"))
(foreign-definition (include-string "definitions.c"))
(foreign-initialization (include-string "initialization.c"))



(define-library (auto scheme process context)
  (export command-line current-directory)
  )

(cutlet (rootlet) 'command-line 'current-directory)
