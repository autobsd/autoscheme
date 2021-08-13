(define-library (auto scheme environment)
  (export make-environment 
	  current-environment
	  environment-ref
	  environment-defined? 
	  environment-update! 
	  environment-import! 
	  environment-only
	  environment-except
	  environment-prefix	  
	  environment-rename

	  define-library 
	  import)
  )

