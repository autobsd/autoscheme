;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside AutoScheme application...\n")

;; void scheme_register_foreign_func(const char *name, foreign_func ff)
(foreign-declaration
"#include <stdio.h>\n"
"pointer my_func( pointer p );\n"
)
(foreign-definition 
"pointer my_func( pointer p )\n"
"{\n"
"printf( \"inside ff:my_func...\\n\" );fflush( stdout );\n"
"return NIL;\n"
"}\n"
)
(foreign-initialization
"scheme_register_foreign_func( \"my-func\", my_func );\n"
)

;; (define-macro (add1 x)
;;   (newline)
;;   (quasiquote '(,(+ x 1))))
 
;; (display (add1 3))(newline)


;; (define environment-defined 
;;   (lambda (environment)
;;     (map car (apply append environment))))

;; (define environment-defined?
;;   (lambda (environment symbol)
;;     (if (member symbol (environment-defined environment)) #t
;; 	#f)))


;; (let ((y 17))
;;   ;; (display (environment-defined (current-environment)))(newline)
;;   ;; (display "environment-defined?: ")(write (environment-defined? (interaction-environment) 'y))(newline)
;;   (display (current-environment))(newline)
;;   )

;; (display (environment? (current-environment)))(newline)


(write (display "hello\n"))(newline)


(my-func)