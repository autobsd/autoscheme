(define-library (auto scheme environment)
  (export make-environment 
	  current-environment
	  environment-ref
	  environment-remove! 
	  environment-defined
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

