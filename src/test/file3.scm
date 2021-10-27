;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme write)
	(auto scheme environment)
	(auto scheme base) 
	;; (except (auto scheme base) lambda)
	(auto scheme directory)
	(scheme process-context)
	(auto scheme macro)
	(auto scheme list)
	(auto scheme lazy)
	(auto scheme memory)
	(auto scheme closure)
	(auto scheme path)
	(auto scheme file)
	(auto scheme macro)
	)

(display "testing:\n")

;; (write (current-directory)) (newline)
;; (write (command-line))(newline)
;; (write (get-environment-variables))(newline)
;; (write (get-environment-variable "HOME"))(newline)

;; (newline)
;; (display "+: ")(write +)(newline)
;; (display "macro?: ")(write (macro? +))(newline)
;; (display "closure?: ")(write (closure? +))(newline)
;; (display "procedure?: ")(write (procedure? +))(newline)


;; (define-macro (mymac x)
;;   `(+ ,x 5)
;;   )
;; (newline)
;; (display "mymac: ")(write mymac)(newline)
;; (display "macro?: ")(write (macro? mymac))(newline)
;; (display "closure?: ")(write (closure? mymac))(newline)
;; (display "procedure?: ")(write (procedure? mymac))(newline)




;; (define (add2 x)
;;   (+ x 2))

;; (newline)
;; (display "add2: ")(write add2)(newline)
;; (display "macro?: ")(write (macro? add2))(newline)
;; (display "closure?: ")(write (closure? add2))(newline)
;; (display "procedure?: ")(write (procedure? add2))(newline)

;; (newline)
;; (display "environment-only: ")(write environment-only)(newline)
;; (display "macro?: ")(write (macro? environment-only))(newline)
;; (display "closure?: ")(write (closure? environment-only))(newline)
;; (display "procedure?: ")(write (procedure? environment-only))(newline)


;; (newline)
;; (define path "/some/path/to/file.scm")
;; (display "path: ")(write path)(newline)
;; (display "path-directory: ")(write (path-directory path))(newline)
;; (gc-verbose #t)
;; (collect-garbage)

;; (write (last-pair '(a b c)))(newline)

;; (write (get-closure-code (lambda () (display "message"))))(newline)

;; (define expression (delay (display "evaluating expression now")(newline)))

;; (display "expression defined")(newline)

;; (force expression)
;; (force expression)

;; (display (environment-defined-symbols (environment-ref (current-environment) (string->symbol "(auto scheme base)"))))(newline)

;; (newline)
;; (define my-param (make-parameter 5 (lambda (x) (+ x 1))))
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param 3): ")(write (my-param 3))(newline)
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param 7): ")(write (my-param 7))(newline)
;; (display "(my-param): ")(write (my-param))(newline)

;; (parameterize ((my-param 2))
;; 	      (display "(my-param): ")(write (my-param))(newline)
;; 	      )

;; (display "(my-param): ")(write (my-param))(newline)


;; (include "file2.scm")


;; (display "x: ")(write x)(newline)



;; (define x (quote (a b . c) 5 5))
;; (display "x: ")(write x)(newline)
;; (display "(list? x): ")(write (list? x))(newline)


;; (write (current-directory))(newline)
;; (write (current-directory "../../../platforms/darwin"))(newline)

;; ;; (with-output-to-file "test.txt" (lambda () (display "test output2")(newline)))

;; (define o-port (open-output-file "test.txt"))
;; (display "test output 3" o-port)(newline o-port)
;; (close-output-port o-port)
;; (display "this is a normal message.\n" (current-output-port))
;; (display "this is an error message!\n" (current-error-port))(newline (current-error-port))
;; (display "this is a normal message.\n" (current-output-port))


;; (display "vector: ")(write (list->vector '(<error-object> "File error - message" irr1 irr2)))(newline)

;; (newline)

;; (call/cc (lambda (return)
;; 	   (define my-handler
;; 	     (lambda (obj)
;; 	       (display "doing something with: ")(write obj)(newline)
;; 	       (return)
;; 	       ))

;; 	   (with-exception-handler 
;; 	    my-handler
;; 	    (lambda ()

;; 	      (display "inside 'my-handler' scope")(newline)

;; 	      (error "File error - message" 'irr1 "irr2" 'irr3)

;; 	      ))
;; 	   ))

;; ;; (raise (list->vector '(<error-object> <file> "File error - message" (irr1 "irr2" irr3))))
;; ;; (raise (list->vector '(<error-object> "File error - message" ())))
;; (display "After exception")(newline)
;; ;; (write #((<file-error> <error-object>) "message" (irr1 "irr2" irr3)))(newline)
;; ;; (write #((<error-object>) "message" (irr1 "irr2" irr3)))(newline)
;; ;; (write #(<error-object> <file> "message" (irr1 "irr2" irr3)))(newline)


;; ;; #(<error-object> #f "File error - message" (irr1 "irr2" irr3))
;; ;; #((<file-error> <error-object>) "message" (irr1 "irr2" irr3))
;; ;; #((<error-object>) "message" (irr1 "irr2" irr3))

;; ;; #(<error-object> "File error - message" (irr1 "irr2" irr3))
;; ;; #(<error-object> <file> "message" (irr1 "irr2" irr3))

;; ;; #(<error-object> File "message" (irr1 "irr2" irr3))
;; ;; #(<error-object> #f "message" (irr1 "irr2" irr3))

;; ;; #(<ERROR-OBJECT> record-type_0 "File error - message" (irr1 "irr2" irr3))




;; (write (gensym))(newline)
;; (write (gensym))(newline)
;; (write (gensym 'record-type_))(newline)


;; (define-record-type <PARE>
;;   (kons x y)
;;   pare?
;;   (x kar set-kar!)
;;   (y kdr))

;; (define my-pare (kons 1 2))
;; (write my-pare)(newline)
;; (write (kar my-pare))(newline)
;; (write (set-kar! my-pare 11))(newline)
;; (write (kar my-pare))(newline)
;; (write (kdr my-pare))(newline)
;; (newline)
;; (write (define my-var 7))(newline)
;; (write my-var)(newline)
;; (write (set! my-var 77))(newline)
;; (write my-var)(newline)

(raise-continuable 5)

(display "after handlers")(newline)


;; (guard (condition
;;              ((assq ’a condition) => cdr)
;;              ((assq ’b condition)))
;;       (raise (list (cons ’a 42))))