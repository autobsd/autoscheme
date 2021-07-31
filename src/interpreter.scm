(define interpret-program
  (lambda (source-files)
    (display "interpetting program from: ")(write source-files)(newline)
    (with-let (inlet 'source-files source-files)
	      (for-each load source-files)
	      )
    
    (exit)
    ))